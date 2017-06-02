(* guestfs-inspection
 * Copyright (C) 2009-2017 Red Hat Inc.
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
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 *)

open Unix
open Printf

open Common_utils

let prog_exists prog =
  try ignore (which prog); true
  with Executable_not_found _ -> false

let commandr prog args =
  if verbose () then
    eprintf "command: %s %s\n"
            prog (String.concat " " args);

  let argv = Array.of_list (prog :: args) in

  let stdout_file, stdout_chan = Filename.open_temp_file "cmd" ".out" in
  let stderr_file, stderr_chan = Filename.open_temp_file "cmd" ".err" in
  let stdout_fd = descr_of_out_channel stdout_chan in
  let stderr_fd = descr_of_out_channel stderr_chan in
  let stdin_fd = openfile "/dev/null" [O_RDONLY] 0 in

  let pid = fork () in
  if pid = 0 then (
    (* Child process. *)
    dup2 stdin_fd stdin;
    close stdin_fd;
    dup2 stdout_fd stdout;
    close stdout_fd;
    dup2 stderr_fd stderr;
    close stderr_fd;

    execvp prog argv
  );

  (* Parent process. *)
  close stdin_fd;
  close stdout_fd;
  close stderr_fd;
  let _, status = waitpid [] pid in
  let r =
    match status with
    | WEXITED i -> i
    | WSIGNALED i ->
       failwithf "external command ‘%s’ killed by signal %d" prog i
    | WSTOPPED i ->
       failwithf "external command ‘%s’ stopped by signal %d" prog i in

  if verbose () then
    eprintf "command: %s returned %d\n" prog r;

  let stdout = read_whole_file stdout_file in
  let stderr = read_whole_file stderr_file in

  if verbose () then (
    if stdout <> "" then (
      eprintf "command: %s: stdout:\n%s" prog stdout;
      if not (String.is_suffix stdout "\n") then eprintf "\n"
    );
    if stderr <> "" then (
      eprintf "command: %s: stderr:\n%s" prog stderr;
      if not (String.is_suffix stderr "\n") then eprintf "\n"
    )
  );

  (* Strip trailing \n from stderr but NOT from stdout. *)
  let stderr =
    let n = String.length stderr in
    if n > 0 && stderr.[n-1] = '\n' then
      String.sub stderr 0 (n-1)
    else
      stderr in

  (r, stdout, stderr)

let command prog args =
  let r, stdout, stderr = commandr prog args in
  if r <> 0 then
    failwithf "%s exited with status %d: %s" prog r stderr;
  stdout

let udev_settle ?filename () =
  let args = ref [] in
  if verbose () then
    push_back args "--debug";
  push_back args "settle";
  (match filename with
   | None -> ()
   | Some filename ->
      push_back args "-E";
      push_back args filename
  );
  let args = !args in
  let r, _, err = commandr "udevadm" args in
  if r <> 0 then
    eprintf "udevadm settle: %s\n" err

let root_device = lazy ((stat "/").st_dev)

let is_root_device_stat statbuf =
  statbuf.st_rdev = Lazy.force root_device

let is_root_device device =
  udev_settle ~filename:device ();
  try
    let statbuf = stat device in
    is_root_device_stat statbuf
  with
    Unix_error (err, func, arg) ->
      eprintf "is_root_device: %s: %s: %s: %s\n"
              device func arg (error_message err);
      false

let proc_unmangle_path path =
  let n = String.length path in
  let b = Buffer.create n in
  let rec loop i =
    if i < n-3 && path.[i] = '\\' then (
      let to_int c = Char.code c - Char.code '0' in
      let v =
        (to_int path.[i+1] lsl 6) lor
        (to_int path.[i+2] lsl 3) lor
        to_int path.[i+3] in
      Buffer.add_char b (Char.chr v);
      loop (i+4)
    )
    else if i < n then (
      Buffer.add_char b path.[i];
      loop (i+1)
    )
    else
      Buffer.contents b
  in
  loop 0

let is_small_file path =
  is_regular_file path &&
    (stat path).st_size <= 2 * 1048 * 1024

(* This function is also available in Common_utils, but it calls
 * error() and uses Guestfs so we cannot use that function here.
 *)
let run_main_and_handle_errors main =
  try main ()
  with
  | Unix.Unix_error (code, fname, "") -> (* from a syscall *)
     eprintf "%s: %s\n" fname (Unix.error_message code);
     exit 1
  | Unix.Unix_error (code, fname, param) -> (* from a syscall *)
     eprintf "%s: %s: %s\n" fname (Unix.error_message code) param;
     exit 1
  | Sys_error msg ->                    (* from a syscall *)
     eprintf "%s\n" msg;
     exit 1
  | Failure msg ->                      (* from failwith/failwithf *)
     eprintf "failure: %s\n" msg;
     exit 1
  | Invalid_argument msg ->             (* probably should never happen *)
     eprintf "internal error: invalid argument: %s\n" msg;
     exit 1
  | Assert_failure (file, line, char) -> (* should never happen *)
     eprintf "internal error: assertion failed at %s, line %d, char %d\n"
             file line char;
     exit 1
  | Not_found ->                        (* should never happen *)
     eprintf "internal error: Not_found exception was thrown\n";
     exit 1
  | exn ->                              (* something not matched above *)
     eprintf "exception: %s\n" (Printexc.to_string exn);
     exit 1
