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

type key = string
type kind =
  | Amain of key Anycache_dlist.node
  | A1in
  | A1out of key Anycache_dlist.node

type t = {
  amain : key Anycache_dlist.t;
  a1in: key Queue.t;
  a1out : key Anycache_dlist.t;
  table : (key, int64 * kind) Hashtbl.t;
  mutable a1in_size :  int64;
  mutable a1out_size : int64;
  mutable amain_size : int64;
  kin: int64;
  kout: int64;
  total_size : int64;
  on_delete : (key -> unit);
}

let create ?(on_delete=ignore) n =
  let kout = Int64.div n 2L in {
    amain = Anycache_dlist.create ();
    a1in = Queue.create ();
    a1out = Anycache_dlist.create ();
    table = Hashtbl.create 1021;
    a1in_size = 0L;
    a1out_size = 0L;
    amain_size = 0L;
    kin = Int64.div n 4L;
    kout = kout;
    total_size = n;
    on_delete
  }

open Anycache_dlist

let remove_key cache key =
  try
    let r = Hashtbl.find cache.table key in
    Hashtbl.remove cache.table key;
    r
  with Not_found -> 0L, A1in

let rec reclaim_a1out cache =
  if cache.a1out_size > cache.kout then begin
    let key = remove (tail cache.a1out) in
    let size, _kind = remove_key cache key in
    cache.a1out_size <- Int64.sub cache.a1out_size size;
    cache.on_delete key;
    reclaim_a1out cache
  end

let add_a1out cache key size =
  let node = Anycache_dlist.add_head cache.a1out key in
  cache.a1out_size <- Int64.add cache.a1out_size size;
  reclaim_a1out cache;
  Hashtbl.replace cache.table key (size, A1out node);
  Logs.debug (fun m -> m "added to a1out: %s (total %Ld)" key cache.a1out_size)

let add_a1in cache key size =
  Queue.push key cache.a1in;
  Hashtbl.replace cache.table key (size, A1in);
  cache.a1in_size <- Int64.add cache.a1in_size size;
  Logs.debug (fun m -> m "added to a1in: %s (total %Ld)" key cache.a1in_size)

let add_main cache key size =
  let node = add_head cache.amain key in
  Hashtbl.replace cache.table key (size, Amain node);
  cache.amain_size <- Int64.add cache.amain_size size;
  Logs.debug (fun m -> m "added to amain: %s (total %Ld)" key cache.amain_size)

let rec reclaim cache =
  let n = Int64.add cache.a1in_size cache.a1out_size |> Int64.add cache.amain_size in
  if n < cache.total_size then ()
  else if cache.a1in_size > cache.kin then begin
    let ykey = Queue.pop cache.a1in in
    let size, _ = remove_key cache ykey in
    cache.a1in_size <- Int64.sub cache.a1in_size size;
    add_a1out cache ykey size;
    reclaim cache
  end else begin
    let y = remove (tail cache.amain) in
    let size, _ = remove_key cache y in
    cache.amain_size <- Int64.sub cache.amain_size size;
    cache.on_delete y;
    (* do not put it on A1out, it hasn't been accessed for a while *)
    reclaim cache
  end

let reclaim_bytes n cache =
  cache.a1in_size <- Int64.add cache.a1in_size n;
  reclaim cache;
  cache.a1in_size <- Int64.sub cache.a1in_size n

let find cache key =
  let size, kind = Hashtbl.find cache.table key in
  begin match kind with
  | Amain node ->
      ignore (remove node);
      Hashtbl.replace cache.table key (size, Amain (add_head cache.amain key));
  | A1in -> ()
  | A1out node ->
      ignore (Anycache_dlist.remove node);
      cache.a1out_size <- Int64.sub cache.a1out_size size;
      add_main cache key size
  end;
  size

let add cache key size =
  try find cache key |> ignore
  with Not_found ->
    reclaim cache;
    add_a1in cache key size

let touch cache key =
  try find cache key |> ignore with Not_found -> ()
