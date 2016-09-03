(* libguestfs
 * Copyright (C) 2016 Red Hat Inc.
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

open Printf
open Unix

let max_futures = ref 8 (* XXX Choose a better default. *)

(* Pending = not yet running, in the pending set
   Running = running, in the running set
   Finished = finished, not in any set *)
type state = Pending | Running | Finished

type 'a _future_t = {
    id : int;                   (* ID (just used for ordering in the sets) *)
    mutable state : state;      (* Current state. *)

    f_wrapper : out_channel -> unit; (* Calls f and writes marshalled result
                                        to [out_channel]. *)
    mutable rfd : file_descr;   (* Read end of the pipe. *)
    mutable pid : int;          (* PID of subprocess. *)

    rbuf : Buffer.t;            (* Marshalled result. *)
    mutable r : 'a option;      (* Cached return value of the function. *)
}
type 'a t = 'a _future_t

module FSet = Set.Make (
    struct
      type t = unit _future_t
      let compare { id = id1 } { id = id2 } = compare id1 id2
    end
  )

(* The implementation uses two sets (and a third implicit state).

   New futures are added to the {!pending} set.  Anything in the
   pending set is not yet running.  The {!state} will be {!Pending}.

   Futures which are running are placed in the {!running} set.
   The state will be {!Running}.  The length of the running set
   is capped at {!max_futures}.

   When a future has finished running, it is taken out of the
   running set.  The state will be {!Finished}, but the result
   is sitting (marshalled) in [rbuf].  When {!force} is called
   the result is unmarshalled into [!r].

   In state {!Finished} the future is not in any set, and so will
   be garbage collected when any references held by the main program
   are dropped (even if force is never called).

   Because of polymorphism we have to cast (using {!Obj.magic})
   the futures to type [unit t] before adding them to the set.
   This is (hopefully!) safe because of the module interface. *)

let pending = ref FSet.empty
let running = ref FSet.empty

let pending_set_not_empty () = not (FSet.is_empty !pending)
let running_set_not_empty () = not (FSet.is_empty !running)

(* Scan the running set looking for futures which have finished,
 * and remove them from the running set.
 *
 * If there is room on the running set, move future(s) from the
 * pending set to the running set and start them running.
 *
 * This call does not block.
 *)
let rec scan_sets () =
  let changed = ref true in
  while !changed && running_set_not_empty () do
    changed := false;
    FSet.iter (
      fun fut ->
        let fds, _, _ = select [fut.rfd] [] [] 0. in
        if fds <> [] then (
          changed := true;
          (* Do the rest of the work finishing off this future. *)
          finish_future fut
        )
    ) !running
  done;

  let max_futures = !max_futures in
  assert (max_futures > 0);

  (* Move futures from the pending set to the running set until
   * the running set contains {!max_futures} members.
   *)
  while FSet.cardinal !running < max_futures && pending_set_not_empty () do
    (* Choose the future with the smallest ID from the pending set. *)
    let fut = FSet.min_elt !pending in
    pending := FSet.remove fut !pending;

    (* Run it and add it to the running set. *)
    let rfd, wfd = pipe () in
    fut.pid <- fork ();
    if fut.pid = 0 then (       (* Child process. *)
      close rfd;
      let chan = out_channel_of_descr wfd in
      fut.f_wrapper chan;
      Pervasives.flush chan;
      (* XXX We'd like to call _exit here, but instead we have to
       * do this hack to avoid OCaml finalizers running.
       *)
      execvp "true" [| "true" |]
    );
    close wfd;
    fut.rfd <- rfd;
    fut.state <- Running;
    running := FSet.add fut !running
  done

(* This helper function does the rest of the work needed to
 * transition a future from Running to Finished.
 *)
and finish_future fut =
  fut.state <- Finished;

  (* Drop the future from the running set. *)
  running := FSet.remove fut !running;

  (* Read serialized result from the subprocess.  We cannot unmarshall
   * the result because we don't know the result type here, which is
   * why all unmarshalling is done by the {!force} function.
   *)
  let rec loop () =
    let buf = Bytes.create 1024 in
    let n = read fut.rfd buf 0 1024 in
    if n > 0 then (
      let s = Bytes.to_string (Bytes.sub buf 0 n) in
      Buffer.add_string fut.rbuf s;
      loop ()
    )
  in
  loop ();
  close fut.rfd;

  (* Clean up the subprocess. *)
  let _, status = waitpid [] fut.pid in
  (match status with
   | WEXITED 0 -> ()
   | WEXITED i ->
      failwith (sprintf "future: subprocess exited with non-zero status (%d)" i)
   | WSIGNALED i | WSTOPPED i ->
      failwith (sprintf "future: subprocess signalled or stopped with signal %d"
                        i)
  );
  fut.pid <- 0

(* This waits until something happens on one of the file descriptors
 * of a running process.
 *)
let wait_event () =
  assert (FSet.cardinal !running > 0);
  let fds = FSet.fold (fun { rfd = rfd } fds -> rfd :: fds) !running [] in
  ignore (select fds [] [] (-1.))

let next_id = ref 0
let get_id () = let id = !next_id in incr next_id; id
let create f =
  (* We have to wrap [f] into a wrapper function which hides the
   * polymorphic return value ['a].  This is because we will be
   * putting the future onto a generic set, where the type of
   * the future is (temporarily) lost.
   *)
  let f_wrapper chan = Marshal.to_channel chan (f ()) [] in

  let fut = {
    id = get_id (); state = Pending;
    r = None; f_wrapper = f_wrapper; rbuf = Buffer.create 1024;
    rfd = stdin; (* just for initialization, any fd will do *)
    pid = 0;
  } in

  (* Add it to the pending set.  If there is room in the running
   * set then scan_sets will immediately move it to running.
   *)
  pending := FSet.add (Obj.magic fut) !pending;
  scan_sets ();
  fut

let try_force fut =
  scan_sets ();

  match fut.r with
  | Some _ as r -> r            (* Finished and result cached. *)
  | None ->
     match fut.state with
     | Pending
     | Running -> None
     | Finished ->
        (* Finished, result is waiting to be unmarshalled and cached. *)
        assert (Buffer.length fut.rbuf > 0);
        let r = Some (Marshal.from_string (Buffer.contents fut.rbuf) 0) in
        Buffer.clear fut.rbuf;
        fut.r <- r;
        r

let rec force fut =
  match try_force fut with
  | Some r -> r
  | None ->
     (* Block until something happens, then repeat. *)
     wait_event ();
     force fut
