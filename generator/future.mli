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

(** A simple implementation of "futures" using forking.

    A future is a deferred function call which is actually executed
    in parallel with the main program.  The function call runs and
    returns some value.  To read the value you have to call {!force}
    on the future, which will block until the function returns.

    The current implementation uses {!Unix.fork}.  Up to {!max_futures}
    simultaneous subprocesses will be created, each running one future.
    Because of the forking, the future cannot modify any state in the rest
    of the program, and should generally be a "pure function".

    https://en.wikipedia.org/wiki/Futures_and_promises *)

type 'a t
(** The future abstract datatype.  ['a] is the return type of the
    function.  To read the return value, call {!force}. *)

val create : (unit -> 'a) -> 'a t
(** [create f] creates a future.  The function [f ()] runs asynchronously.
    The return value of [f] is not available until you call
    {!force} on the future.

    Note that [f] may not start to run immediately.  This implementation
    will not allow more than {!max_futures} to run in parallel, so if
    there are already this many futures running then the start up of
    [f] will be delayed.

    Because of the forking implementation, [f] cannot modify any
    state in the program.

    The current implementation does not handle exceptions properly,
    so it is best if [f] does not raise exceptions (it will probably
    cause the program to crash).  If [f] needs to return an error,
    encode that in the return type. *)

val force : 'a t -> 'a
(** Read the return value of the future.  This will block until the
    function finishes running.

    You can call force more than once.  This does {i not} run the
    future again, it just returns the cached value. *)

val try_force : 'a t -> 'a option
(** This is like {!force}, but it does not block.  If the future
    is still running, it returns [None]. *)

val max_futures : int ref
(** The maximum number of futures that may run in parallel.  This
    is initialized to a suitable value.  You can also modify it
    at any time if you want, but reducing the value will not
    take effect until futures which are currently running naturally
    finish. *)
