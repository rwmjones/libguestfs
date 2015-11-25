/* libguestfs -*- text -*-
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
 */

%{
open Types
%}

%token <string> STRING /* string literal */
%token <string> UID    /* uppercase identifier */
%token <string> LID    /* lowercase identifier */

%token TRUE            /* true (keyword) */
%token FALSE           /* false (keyword) */

%token LPAREN RPAREN   /* ( ... ) */
%token <string> CODE   /* {{ .. }} containing C code */
%token STAR            /* * (zero or more rows) */
%token QUESTION        /* ? (zero or one row) */
%token PLUS            /* + (one or more rows) */
%token DOT             /* . */
%token IMPLIC          /* :- (implication) */
%token COMMA           /* , (AND operator) */
%token SEMI            /* ; (OR operator) */
%token NOT             /* ! */
%token EQUALS          /* = */

/* These operators are arranged from lowest to highest precedence. */
%left IMPLIC
%left SEMI
%left COMMA
%nonassoc NOT

%start chunk
%type <Types.chunk> chunk
%type <Types.rule> rule

%%

chunk:    CODE
            { CodeChunk { code = $1;
                          code_loc = symbol_start_pos ();
                          code_fn = "" } }
        | rule
            { RuleChunk $1 }
        ;

rules:    /* empty */
            { [] }
        | rule rules
            { $1 :: $2 }

rule:     head DOT
            { { head = $1; body = True;
                rule_loc = symbol_start_pos (); rule_fn = "" } }
        | head IMPLIC body DOT
            { { head = $1; body = $3;
                rule_loc = symbol_start_pos (); rule_fn = "" } }
        ;

head:   term
            { $1 }
        ;

term:     UID
            { { term_name = $1; term_args = [] } }
        | UID LPAREN term_args RPAREN
            { { term_name = $1; term_args = $3 } }
        ;

term_args:
          term_arg
            { [ $1 ] }
        | term_arg COMMA term_args
            { $1 :: $3 }
        ;

term_arg:
          LID
            { Variable $1 }
        | STRING
            { Constant $1 }
        ;

body:   expr
            { $1 }
        ;

expr:     TRUE
            { True }
        | FALSE
            { False }
        | term
            { Term $1 }
        | CODE
            { BoolCode { code = $1;
                         code_loc = symbol_start_pos ();
                         code_fn = "" } }
        | LPAREN result_bindings RPAREN EQUALS CODE
            { AssignCode ($2, RowsOne,
                         { code = $5;
                           code_loc = symbol_start_pos (); code_fn = "" }) }
        | LPAREN result_bindings RPAREN STAR EQUALS CODE
            { AssignCode ($2, RowsZeroOrMore,
                         { code = $6;
                           code_loc = symbol_start_pos (); code_fn = "" }) }
        | LPAREN result_bindings RPAREN QUESTION EQUALS CODE
            { AssignCode ($2, RowsZeroOrOne,
                         { code = $6;
                           code_loc = symbol_start_pos (); code_fn = "" }) }
        | LPAREN result_bindings RPAREN PLUS EQUALS CODE
            { AssignCode ($2, RowsOneOrMore,
                         { code = $6;
                           code_loc = symbol_start_pos (); code_fn = "" }) }
        | NOT term
            { Not $2 }
        | expr COMMA expr
            { And ($1, $3) }
        | expr SEMI expr
            { Or ($1, $3) }
        | LPAREN expr RPAREN
            { $2 }
        ;

result_bindings:
          LID
            { [ $1 ] }
        | LID COMMA result_bindings
            { $1 :: $3 }
