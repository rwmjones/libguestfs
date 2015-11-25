(* libguestfs
 * Copyright (C) 2009-2016 Red Hat Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 *)

(* This is the compiler that turns inspection rules into C code. *)

open Printf

open Utils
open Types
open Pr
open Docstrings

module StringSet = Set.Make (String)

let (//) = Filename.concat

type env = {
  free_vars : string list;     (* Variables which are free in the rule. *)
  assign_vars : string list;   (* Variables assigned by C code. *)
  env_struct : string;         (* Name of the C environment struct. *)
}

let rec compile filename () =
  let prologue, rules, epilogue = parse filename in
  type_check filename rules;

  generate_header ~inputs:[filename; "generator/rules_compiler.ml"]
                  CStyle GPLv2plus;

  pr "\
#include <config.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <unistd.h>
#include <errno.h>
#include <error.h>

#include \"gl_oset.h\"
#include \"gl_xoset.h\"
#include \"gl_array_oset.h\"

#include \"cleanups.h\"
#include \"rules.h\"
#include \"guestfs-internal-all.h\"

";

  (match prologue with
   | Some code -> insert_literal_code filename code
   | None -> ()
  );

  pr "\
/* Disable a few warnings, so we can take a few short-cuts with the
 * generated code.
 */
#pragma GCC diagnostic ignored \"-Wunused-variable\"
#pragma GCC diagnostic ignored \"-Wunused-macros\"
#pragma GCC diagnostic ignored \"-Wunused-function\"

static gl_oset_t
new_string_set (void)
{
  /* Note that we don't need a dispose function because all the
   * strings added to the set will be \"owned\" by other code, either
   * static (all_strings) or owned by facts.
   */
  return gl_oset_create_empty (GL_ARRAY_OSET,
                               (gl_setelement_compar_fn) strcmp, NULL);
}

static void
add_all_strings (gl_oset_t set)
{
  size_t i;

  for (i = 0; all_strings[i] != NULL; ++i)
    gl_oset_add (set, all_strings[i]);
}

";

  get_all_strings filename rules;

  (* Give each rule a unique number.  The number is used for rule
   * function names, environments, code functions and so on.
   * eg: 'rule_0 ()', 'struct rule_0_env', 'rule_0_code_0 ()'.
   *)
  iteri (
    fun i rule ->
      rule.rule_fn <- sprintf "rule_%d" i
  ) rules;

  (* Create the environment struct for each rule.  This contains all
   * the variables either consumed or set by the function.
   *)
  let rules =
    List.map (
      fun rule ->
        let env = compile_rule_environment filename rule in
        (rule, env)
    ) rules in

  (* Write all C code snippets to functions. *)
  List.iter (
    fun (rule, env) ->
      let j = ref 0 in
      let rec loop = function
        | And (e1, e2) | Or (e1, e2) -> loop e1; loop e2
        | BoolCode code ->
           code.code_fn <- sprintf "%s_code_%d" rule.rule_fn !j;
           incr j;
           compile_bool_code filename rule env code
        | AssignCode (vs, row_count, code) ->
           code.code_fn <- sprintf "%s_code_%d" rule.rule_fn !j;
           incr j;
           compile_assign_code filename rule env vs row_count code
        | Term _ | Not _ | True | False -> ()
      in
      loop rule.body
  ) rules;

  (* Compile all rules into functions. *)
  List.iter (fun (rule, env) -> compile_rule filename rule env) rules;

  pr "\
void
rules (void)
{
  clear_true_facts ();
  clear_false_facts ();

  /* Loop over all the rules until no more true facts can be added. */
  for (;;) {
    size_t nr_true_facts = count_true_facts ();

";

  List.iter (
    fun (rule, env) ->
      pr "    if (verbose)\n";
      pr "      printf (\"trying rule %%s\\n\", %S);\n"
         (string_of_term rule.head);
      pr "    %s ();\n" rule.rule_fn;
      pr "\n";
  ) rules;

  pr "    /* Added a true fact during this iteration? */
    if (nr_true_facts == count_true_facts ())
      break;
  } /* for (;;) */
}

";

  (match epilogue with
   | Some code -> insert_literal_code filename code
   | None -> ()
  );

  pr "/* EOF */\n"

and insert_literal_code filename code =
  (* XXX This function gets the line number wrong. *)
  let lineno = code.code_loc.Lexing.pos_lnum in
  pr "#line %d \"%s\"\n" lineno filename;
  pr "%s\n" code.code

and get_all_strings filename rules =
  let rec loop = function
    | True | False | BoolCode _ | AssignCode _ -> []
    | And (e1, e2) | Or (e1, e2) -> loop e1 @ loop e2
    | Term term | Not term -> get_term_strings term
  and get_term_strings { term_args = args } =
    filter_map (function Variable _ -> None | Constant s -> Some s) args
  in
  let all_strings =
    List.map (fun rule -> get_term_strings rule.head @ loop rule.body) rules in
  let all_strings = List.concat all_strings in
  let all_strings = sort_uniq all_strings in
  pr "const char *all_strings[] = {\n";
  pr "    ";
  let col = ref 0 in
  List.iter (
    fun s ->
      let len = String.length s in
      if !col + len + 4 >= 72 then (
        col := 0;
        pr "\n    "
      );
      pr "%S, " s;
      col := !col + len + 4;
  ) all_strings;
  if !col > 0 then pr "\n";
  pr "    NULL\n";
  pr "};\n";
  pr "\n"

(* Work the environment of a single rule.  Also write out the
 * corresponding struct to the C file.
 *)
and compile_rule_environment filename rule =
  (* The name of the C struct. *)
  let env_struct = sprintf "%s_env" rule.rule_fn in

  (* Divide all the variables which appear in the rule into:
   *  - ones which we have to search for [free_vars],
   *  - ones which are going to be returned by a C expression within
   *    the body [assign_vars].
   * We can do this statically.
   * These sets are non-overlapping, so we just need to check which
   * variables are returned by C expressions, and do an additional
   * check that no C expressions are returning the same variable.
   *)
  (* Get the complete list of vars ... *)
  let free_vars = Hashtbl.create 13 in
  (* ... from the head *)
  List.iter (
    function
    | Variable v ->
       if Hashtbl.mem free_vars v then (
         eprintf "%s: variable '%s' appears two or more times in a rule\n"
                 filename v;
         exit 1
       );
       Hashtbl.add free_vars v 1
    | Constant _ -> ()
  ) rule.head.term_args;
  (* ... from the body *)
  let rec loop = function
    | And (e1, e2) | Or (e1, e2) -> loop e1; loop e2
    | Term { term_args = args } | Not { term_args = args } ->
       List.iter (
         function
         | Variable v -> Hashtbl.replace free_vars v 1
         | Constant _ -> ()
       ) args
    | True | False
    | BoolCode _ | AssignCode _ -> ()
  in
  loop rule.body;

  let assign_vars = Hashtbl.create 13 in
  let rec loop = function
    | True | False | Term _ | Not _ | BoolCode _ -> ()
    | And (e1, e2) | Or (e1, e2) -> loop e1; loop e2
    | AssignCode (vs, _, _) ->
       List.iter (
         fun v ->
           Hashtbl.remove free_vars v;
           if Hashtbl.mem assign_vars v then (
             eprintf "%s: variable '%s' appears two or more times in a C assignment expression in a rule\n"
                     filename v;
             exit 1
           );
           Hashtbl.add assign_vars v 1
       ) vs
  in
  loop rule.body;
  let free_vars = Hashtbl.fold (fun k _ ks -> k :: ks) free_vars [] in
  let assign_vars = Hashtbl.fold (fun k _ ks -> k :: ks) assign_vars [] in

  (* Write out the C struct. *)
  pr "/* Environment struct for rule %s */\n" (string_of_term rule.head);
  pr "struct %s {\n" env_struct;
  if free_vars <> [] then (
    pr "  /* free variables */\n";
    List.iter (pr "  char *%s;\n") free_vars
  );
  if assign_vars <> [] then (
    pr "  /* (rows of) variables assigned by C code */\n";
    pr "  size_t nr_rows;\n";
    List.iter (pr "  char **%s;\n") assign_vars
  );
  pr "};\n";
  pr "\n";

  (* Return the OCaml env. *)
  { free_vars = free_vars;
    assign_vars = assign_vars;
    env_struct = env_struct; }

(* Compile a single rule to C code. *)
and compile_rule filename rule env =
  (* For each free variable we need to find the possible values for that
   * variable.  If they appear within the body in a term like
   * 'Foo(var)' then we can just look for matching facts and add
   * them (at runtime).  If they don't, then we start with the list
   * of all strings culled from the source + all strings from all facts.
   *)
  let free_vars = List.map (
    fun v ->
      let fact_lookups = ref [] in
      let rec loop = function
        | True | False | BoolCode _ | AssignCode _ -> ()
        | And (e1, e2) | Or (e1, e2) -> loop e1; loop e2
        | Term { term_name = term_name; term_args = args }
        | Not { term_name = term_name; term_args = args } ->
           (* If this term contains this variable at some position,
            * then save that in the list of 'facts'.
            *)
           iteri (
             fun arg_i ->
               function
               | Variable v' when v = v' ->
                  fact_lookups := (term_name, arg_i) :: !fact_lookups
               | Variable _ | Constant _ -> ()
           ) args
      in
      loop rule.body;
      let fact_lookups = sort_uniq !fact_lookups in

      v, fact_lookups
  ) env.free_vars in

  pr "/* %s */\n" (string_of_term rule.head);
  pr "static void\n";
  pr "%s (void)\n" rule.rule_fn;
  pr "{\n";
  pr "  struct %s env;\n" env.env_struct;
  pr "  bool added;\n";
  pr "  size_t i;\n";
  List.iter (
    fun (v, _) ->
      pr "  gl_oset_t search_%s;\n" v;
      pr "  gl_oset_iterator_t iter_%s;\n" v;
  ) free_vars;
  pr "\n";

  (* This is an optimization: If the rule contains no free variables,
   * we only need to run it once.  This even applies if there are
   * assigned variables, because the C code is supposed to be pure,
   * ie. produce the same result every time it is called.
   *)
  if free_vars = [] then (
    pr "  /* Because this rule contains no free variables, we only need\n";
    pr "   * to evaluate it once.  This applies even if the rule runs\n";
    pr "   * C code (see 'C code memoization' in guestfs-inspection(8)\n";
    pr "   * for an explanation of why this is so).\n";
    pr "   */\n";
    pr "  static bool called = false;\n";
    pr "  if (called)\n";
    pr "    return;\n";
    pr "  called = true;\n";
    pr "\n";
  );

  if free_vars <> [] then
    pr "  /* Build the sets we will use for searching each free variable. */\n";
  List.iter (
    function
    | v, [] ->
      (* The variable doesn't appear in any expressions, so
       * add a note to the source.  Maybe emit a compiler warning? XXX
       *)
       pr "  search_%s = new_string_set ();\n" v;
       pr "  /* Warning: variable '%s' is underspecified, so we will\n" v;
       pr "   * search over all strings from the source and all facts.\n";
       pr "   */\n";
       pr "  add_all_strings (search_%s);\n" v;
       pr "  add_all_fact_strings (search_%s);\n" v;
       pr "\n"
    | v, fact_lookups ->
       pr "  search_%s = new_string_set ();\n" v;
       List.iter (
         fun (term_name, arg_i) ->
           pr "  add_strings_from_facts (search_%s, %S, %d);\n"
              v term_name arg_i
       ) fact_lookups;
       pr "\n"
  ) free_vars;

  (* Do a cartesian search over all [free_vars], substituting each set of
   * variables, and evaluating the body.  If it evaluates to true,
   * then we will add a new true fact!  (Or maybe several if we are
   * dealing with a list assignment [()*={{...}}]).  If it evaluates
   * to false, we add a false fact.  It's also possible that we
   * cannot evaluate the rule at all, because it contains unknown
   * facts, in which case we end up adding NO new facts.
   *)
  if free_vars <> [] then (
    pr "  /* Perform cartesian search over free variables. */\n";

    List.iter (
      fun (v, _) ->
        pr "  iter_%s = gl_oset_iterator (search_%s);\n" v v;
        pr "  while (gl_oset_iterator_next (&iter_%s,\n" v;
        pr "                                (const void **)&env.%s)) {\n" v;
    ) free_vars;

  ) else (
    (* If there are no free_vars, then we have to add a dummy loop
     * around the next code so that the 'continue' statement can be used.
     *)
    pr "  do {\n";
  );

  (* Initialize any assign_vars in the env struct.  Note that the
   * free_vars are initialized by the iterator loops above.
   *)
  List.iter (pr "  env.%s = NULL;\n") env.assign_vars;
  if env.assign_vars <> [] then pr "  env.nr_rows = 0;\n";

  (* We can only do this optimization if assign_vars = [],
   * because we don't know what the C code (returning those vars)
   * may give us yet.  XXX Actually we could be looser with this:
   * we only need to check that the head term contains no assigned
   * variables.
   *)
  if env.assign_vars = [] then (
    pr "  {\n";
    pr "    /* If the fact already exists, don't bother doing any work. */\n";
    pr "    CREATE_FACT (fact, %S, %d"
       rule.head.term_name (List.length rule.head.term_args);
    List.iter (function
                | Variable v -> pr ", env.%s" v
                | Constant s -> pr ", %S" s)
              rule.head.term_args;
    pr ");\n";
    pr "\n";
    pr "    if (is_fact (true, fact) || is_fact (false, fact))\n";
    pr "      continue;\n";
    pr "  }\n";
    pr "\n";
  );

  (* Evaluate the expression on the right hand side. *)
  let rec eval result = function
    | True ->
       pr "  %s = 1;\n" result
    | False ->
       pr "  %s = 0;\n" result
    | BoolCode code ->
       pr "  %s = %s (&env);\n" result code.code_fn
    | AssignCode (_, _, code) ->
       pr "  %s (&env);\n" code.code_fn;
       (* The result of AssignCode is always true (else it would
        * have exited in the call above).  Hence:
        *)
       pr "  %s = 1;\n" result
    | And (e1, e2) ->
       let re1 = sprintf "r_%d" (unique ()) in
       pr "  int %s;\n" re1;
       eval re1 e1;
       pr "  if (%s != 1)\n" re1;
       pr "    %s = %s;\n" result re1;
       pr "  else {\n";
       let re2 = sprintf "r_%d" (unique ()) in
       pr "    int %s;\n" re2;
       eval re2 e2;
       pr "    %s = %s;\n" result re2;
       pr "  }\n";
    | Or (e1, e2) ->
       let re1 = sprintf "r_%d" (unique ()) in
       pr "  int %s;\n" re1;
       eval re1 e1;
       pr "  if (%s == 1)\n" re1;
       pr "    %s = %s;\n" result re1;
       pr "  else {\n";
       let re2 = sprintf "r_%d" (unique ()) in
       pr "    int %s;\n" re2;
       eval re2 e2;
       pr "    %s = %s;\n" result re2;
       pr "  }\n";
    | Term term ->
       pr "  {\n";
       pr "    CREATE_FACT (fact, %S, %d"
          term.term_name (List.length term.term_args);
       List.iter (
         function
         | Variable v -> pr ", env.%s" v
         | Constant s -> pr ", %S" s
       ) term.term_args;
       pr ");\n";
       pr "    %s = is_fact (true, fact);\n" result;
       pr "  }\n";
    | Not term ->
       pr "  {\n";
       pr "    CREATE_FACT (fact, %S, %d"
          term.term_name (List.length term.term_args);
       List.iter (
         function
         | Variable v -> pr ", env.%s" v
         | Constant s -> pr ", %S" s
       ) term.term_args;
       pr ");\n";
       pr "    %s = is_fact (false, fact);\n" result;
       pr "  }\n";
  in
  pr "  /* Evaluate the RHS of the rule with this assignment of variables. */\n";
  pr "  int result;\n";
  eval "result" rule.body;
  pr "  if (result == -1) /* not determined */ continue;\n";
  let make_fact ?i ?(indent = 2) () =
    let indent = spaces indent in
    pr "%sCREATE_FACT (fact, %S, %d"
       indent rule.head.term_name (List.length rule.head.term_args);
    List.iter (
      function
      | Variable v ->
         if not (List.mem v env.assign_vars) then
           pr ", env.%s" v
         else (
           let i = match i with Some i -> i | None -> assert false in
           pr ", env.%s[%s]" v i
         )
      | Constant s -> pr ", %S" s
    ) rule.head.term_args;
    pr ");\n";
  in
  pr "  if (result > 0) /* true */ {\n";
  if env.assign_vars = [] then (
    make_fact ~indent:4 ();
    pr "    added = add_fact (true, fact);\n";
    pr "    if (added && verbose) {\n";
    pr "      printf (\"added new fact \");\n";
    pr "      print_fact (true, fact, stdout);\n";
    pr "      printf (\"\\n\");\n";
    pr "    }\n";
  ) else (
    pr "    for (i = 0; i < env.nr_rows; ++i) {\n";
    make_fact ~i:"i" ~indent:6 ();
    pr "      added = add_fact (true, fact);\n";
    pr "      if (added && verbose) {\n";
    pr "        printf (\"added new fact \");\n";
    pr "        print_fact (true, fact, stdout);\n";
    pr "        printf (\"\\n\");\n";
    pr "      }\n";
    pr "    }\n";
  );
  pr "  }\n";
  pr "  if (result == 0) /* false */ {\n";
  if env.assign_vars = [] then (
    make_fact ~indent:4 ();
    pr "\n";
    pr "    added = add_fact (false, fact);\n";
    pr "    if (added && verbose) {\n";
    pr "      printf (\"added new fact \");\n";
    pr "      print_fact (false, fact, stdout);\n";
    pr "      printf (\"\\n\");\n";
    pr "    }\n";
  ) else (
    pr "    for (i = 0; i < env.nr_rows; ++i) {\n";
    make_fact ~i:"i" ~indent:6 ();
    pr "      added = add_fact (false, fact);\n";
    pr "      if (added && verbose) {\n";
    pr "        printf (\"added new fact \");\n";
    pr "        print_fact (false, fact, stdout);\n";
    pr "        printf (\"\\n\");\n";
    pr "      }\n";
    pr "    }\n";
  );
  pr "  }\n";

  (* Free any assign_vars.  The free_vars don't have to be freed
   * because the iterator loop handles them.
   *)
  List.iter (
    fun v ->
      pr "  for (i = 0; i < env.nr_rows; ++i)\n";
      pr "    free (env.%s[i]);\n" v;
      pr "  free (env.%s);\n" v
  ) env.assign_vars;

  if free_vars <> [] then (
    List.iter (
      fun (v, _) ->
        pr "  }\n";
        pr "  gl_oset_iterator_free (&iter_%s);\n" v
    ) (List.rev free_vars);
  ) else (
    pr "  } while (0);\n";
  );
  pr "\n";

  List.iter (
    function
    | v, _ ->
       pr "  gl_oset_free (search_%s);\n" v
  ) free_vars;

  pr "}\n";
  pr "\n"

(* Compile a BoolCode snippet from a rule into a function. *)
and compile_bool_code filename rule env code =
  (* Create a function which wraps the C code. *)
  let code_wrapper_fn = sprintf "%s_wrapper" code.code_fn in
  List.iter (fun v -> pr "#define %s (_env->%s)\n" v v) env.free_vars;
  pr "static int\n";
  pr "%s (struct %s *_env)\n" code_wrapper_fn env.env_struct;
  pr "{\n";
  insert_literal_code filename code;
  pr "}\n";
  List.iter (pr "#undef %s\n") env.free_vars;
  pr "\n";

  (* Create the function itself. *)
  pr "static int\n";
  pr "%s (struct %s *env)\n" code.code_fn env.env_struct;
  pr "{\n";
  pr "  int r;\n";
  pr "\n";
  pr "  if (verbose)\n";
  pr "    printf (\"running C function %%s:%%d\\n\",\n";
  pr "            \"%s\", %d);\n" filename code.code_loc.Lexing.pos_lnum;
  pr "\n";
  pr "  r = %s (env);\n" code_wrapper_fn;
  pr "\n";
  pr "  /* If the C function returns -1, it causes us to exit at once. */\n";
  pr "  if (r == -1)\n";
  pr "    error (EXIT_FAILURE, 0,\n";
  pr "           \"%%s:%%d: C function failed - see earlier errors\",\n";
  pr "           \"%s\", %d);\n"
     filename code.code_loc.Lexing.pos_lnum;
  pr "\n";
  pr "  return r;\n";
  pr "}\n";
  pr "\n";

(* Compile assignment code (AssignCode) snippet into a function. *)
and compile_assign_code filename rule env vs row_count code =
  (* Create a function for setting a row in the result. *)
  let set_vars_fn = sprintf "%s_set_row" code.code_fn in
  let set_vars_alias = sprintf "set_%s" (String.concat "_" vs) in
  pr "static void\n";
  pr "%s (struct %s *_env, %s)\n"
     set_vars_fn env.env_struct
     (String.concat ", "
                    (List.map (sprintf "const char *%s") vs));
  pr "{\n";
  pr "  size_t _i = _env->nr_rows;\n";
  pr "\n";
  List.iter (
    fun v ->
      pr "  _env->%s = realloc (_env->%s, (_i+1) * sizeof (char *));\n" v v;
      pr "  if (_env->%s == NULL)\n" v;
      pr "    error (EXIT_FAILURE, errno, \"realloc\");\n";
      pr "  _env->%s[_i] = strdup (%s);\n" v v;
      pr "  if (_env->%s[_i] == NULL)\n" v;
      pr "    error (EXIT_FAILURE, errno, \"strdup\");\n"
  ) vs;
  pr "  _env->nr_rows++;\n";
  pr "}\n";
  pr "\n";

  (* Create a function which wraps the C code. *)
  let code_wrapper_fn = sprintf "%s_wrapper" code.code_fn in
  List.iter (fun v -> pr "#define %s (_env->%s)\n" v v) env.free_vars;
  pr "#define %s(%s) %s (_env, %s)\n"
     set_vars_alias (String.concat ", " vs)
     set_vars_fn (String.concat ", " (List.map (fun v -> "("^v^")") vs));
  pr "static int\n";
  pr "%s (struct %s *_env)\n" code_wrapper_fn env.env_struct;
  pr "{\n";
  insert_literal_code filename code;
  pr "}\n";
  List.iter (pr "#undef %s\n") env.free_vars;
  pr "#undef %s\n" set_vars_alias;
  pr "\n";

  (* Create the function itself. *)
  pr "static void\n";
  pr "%s (struct %s *env)\n" code.code_fn env.env_struct;
  pr "{\n";
  pr "  int r;\n";
  pr "\n";
  pr "  if (verbose)\n";
  pr "    printf (\"running C function %%s:%%d\\n\",\n";
  pr "            \"%s\", %d);\n" filename code.code_loc.Lexing.pos_lnum;
  pr "\n";
  pr "  r = %s (env);\n" code_wrapper_fn;
  pr "\n";
  pr "  /* If the C function returns -1, it causes us to exit at once. */\n";
  pr "  if (r == -1)\n";
  pr "    error (EXIT_FAILURE, 0,\n";
  pr "           \"%%s:%%d: C function failed - see earlier errors\",\n";
  pr "           \"%s\", %d);\n"
     filename code.code_loc.Lexing.pos_lnum;
  pr "\n";
  pr "  /* Check the set_* function was called the expected number\n";
  pr "   * of times.\n";
  pr "   */\n";
  (match row_count with
   | RowsOne ->
      pr "  if (env->nr_rows != 1)\n"
   | RowsZeroOrMore ->
      pr "  if (0) /* no check necessary for (var)* assignment */\n"
   | RowsZeroOrOne ->
      pr "  if (env->nr_rows > 1)\n"
   | RowsOneOrMore ->
      pr "  if (env->nr_rows < 1)\n"
  );
  pr "    error (EXIT_FAILURE, 0,\n";
  pr "           \"%%s:%%d: C function called %%s incorrect number of times (%%zu)\",\n";
  pr "           \"%s\", %d, \"%s\", env->nr_rows);\n"
     filename code.code_loc.Lexing.pos_lnum set_vars_alias;
  pr "}\n";
  pr "\n";

(* Parse the input. *)
and parse filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let chunks = ref [] in
  (try
      while true do
        let chunk = Rules_parser.chunk Rules_scanner.token lexbuf in
        chunks := chunk :: !chunks
      done
   with
   | End_of_file -> ()
   | Rules_scanner.Error (msg, _, lineno, charno) ->
      eprintf "%s: %d: %d: %s\n" filename lineno charno msg;
      exit 1
   | Parsing.Parse_error ->
      let p = Lexing.lexeme_start_p lexbuf in
      eprintf "%s: %d: %d: syntax error\n"
              filename
              p.Lexing.pos_lnum
              (p.Lexing.pos_cnum - p.Lexing.pos_bol);
      exit 1
  );

  (* Allow only the first and last chunk to be code (optional prologue
   * and epilogue).  The rest must be rules.
   *)
  let rev_chunks = !chunks in

  let epilogue, rev_chunks =
    match rev_chunks with
    | CodeChunk epilogue :: chunks -> Some epilogue, chunks
    | chunks -> None, chunks in

  let chunks = List.rev rev_chunks in

  let prologue, chunks =
    match chunks with
    | CodeChunk prologue :: chunks -> Some prologue, chunks
    | chunks -> None, chunks in

  let rules = List.map (
    function
    | RuleChunk rule -> rule
    | CodeChunk { code_loc = code_loc } ->
       eprintf "%s: %d: syntax error: prologue and epilogue can only appear at the beginning or end of the input file\n"
               filename code_loc.Lexing.pos_lnum;
       exit 1
  ) chunks in

  prologue, rules, epilogue

(* Minimal type checking. *)
and type_check filename rules =
  check_term_rhs filename rules;
  check_term_arity filename rules

(* If a term appears on the right hand side in any expression, then
 * the term must also appear on the left hand side of a rule.
 *)
and check_term_rhs filename rules =
  let names = List.map (fun { head = { term_name = name } } -> name) rules in
  let names =
    List.fold_left (fun set x -> StringSet.add x set) StringSet.empty names in

  let errors = ref 0 in
  List.iter (
    fun { body = body } ->
      visit_terms (
        fun { term_name = name } ->
          if not (StringSet.mem name names) then (
            eprintf "%s: '%s' appears in a rule expression, but does not appear on the left hand side of any rule.  Maybe there is a typo?\n"
                    filename name;
            incr errors
          )
      ) body
  ) rules;
  if !errors > 0 then exit 1

(* Check the arity of terms is the same wherever they appear. *)
and check_term_arity filename rules =
  let hash = Hashtbl.create (List.length rules) in (* name -> arity *)

  let errors = ref 0 in

  let check_arity { term_name = name; term_args = args } =
    let arity = List.length args in
    try
      let expected_arity = Hashtbl.find hash name in
      if arity <> expected_arity then (
        eprintf "%s: '%s' has different number of parameters (has %d, expected %d).  It must have the same number of parameters throughout the program.\n"
                filename name arity expected_arity;
        incr errors
      )
    with
      (* The first time we've seen this term. *)
      Not_found -> Hashtbl.add hash name arity
  in

  List.iter (
    fun { head = head; body = body } ->
      check_arity head;
      visit_terms check_arity body
  ) rules;

  if !errors > 0 then exit 1

and visit_terms f = function
  | And (e1, e2)
  | Or (e1, e2) -> visit_terms f e1; visit_terms f e2
  | Term t
  | Not t -> f t
  | True | False | BoolCode _ | AssignCode _ -> ()

and unique =
  let i = ref 0 in
  fun () -> incr i; !i
