(******************************************************************************)
(* Copyright (c) 2014-2016 Skylable Ltd. <info-copyright@skylable.com>        *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

type 'a node = {
  v: 'a;
  parent: 'a t;
  mutable prev: 'a node option;
  mutable next: 'a node option;
}
and 'a t = {
  mutable head: 'a node option;
  mutable tail: 'a node option;
}

let create () = {
  head = None;
  tail = None;
}

let tail l = match l.tail with
| None -> invalid_arg "list is empty" (*BISECT-IGNORE*)
| Some tail -> tail

let add_head l v =
  let node = { v = v; prev = None; next = l.head; parent = l } in
  begin match l.head with
  | None -> l.tail <- Some node
  | Some old_head ->
      old_head.prev <- Some node;
  end;
  l.head <- Some node;
  node;;

let remove v =
  (* remove from double-linked list *)
  begin match v.prev with
  | None ->
      (* this was head *)
      v.parent.head <- v.next;
  | Some prev ->
      prev.next <- v.next
  end;
  begin match v.next with
  | None ->
      (* this was tail *)
      v.parent.tail <- v.prev;
  | Some next ->
      next.prev <- v.prev
  end;
  v.v
;;
