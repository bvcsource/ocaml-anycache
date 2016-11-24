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

(* 2Q: A Low Overhead High Performance Buffer Management Replacement Algorithm
 * Theodore Johnson, Dennis Shasha
 * 1994
*)

module Make(Key:Map.OrderedType) = struct
  module KMap = Map.Make(Key)
  type 'a kind =
    | Amain of 'a * Key.t Anycache_dlist.node
    | A1in of 'a
    | A1out of 'a * Key.t Anycache_dlist.node

  (* TODO: use bytes for accounting! *)
  type 'a cache = {
    amain: Key.t Anycache_dlist.t;
    a1in: (Key.t * 'a) Queue.t;
    a1out: Key.t Anycache_dlist.t;
    mutable map: 'a kind KMap.t;
    mutable a1in_size: int;
    mutable a1out_size: int;
    mutable amain_size: int;
    kin: int;
    kout: int;
    total_size: int;
  }

  let create n =
    let kout = n / 4 in {
      amain = Anycache_dlist.create ();
      a1in = Queue.create ();
      a1out = Anycache_dlist.create ();
      map = KMap.empty;
      a1in_size = 0;
      a1out_size = 0;
      amain_size = 0;
      kin = n / 4;
      kout = kout;
      total_size = n
    }

  open Anycache_dlist

  let has_room cache =
    cache.a1in_size + cache.a1out_size + cache.amain_size < cache.total_size

  let add_a1out cache key value =
    let node = A1out (value, Anycache_dlist.add_head cache.a1out key) in
    cache.a1out_size <- cache.a1out_size + 1;
    cache.map <- KMap.add key node cache.map

  let add_a1in cache key value =
    Queue.push (key,value) cache.a1in;
    let element = A1in value in
    cache.map <- KMap.add key element cache.map;
    cache.a1in_size <- cache.a1in_size + 1

  let add_main cache key value =
    let node = add_head cache.amain key in
    let element = Amain (value, node) in
    cache.map <- KMap.add key element cache.map;
    cache.amain_size <- cache.amain_size + 1;
    if cache.amain_size*2 > cache.total_size then begin
      let y = remove (tail cache.amain) in
      cache.map <- KMap.remove y cache.map;
      cache.amain_size <- cache.amain_size - 1;
      (* do not put it on A1out, it hasn't been accessed for a while *)
      (* assert (has_room cache) *)
    end

  let reclaim cache =
    if cache.a1in_size > cache.kin then begin
      let ykey,yval = Queue.pop cache.a1in in
      cache.a1in_size <- cache.a1in_size - 1;
      cache.map <- KMap.remove ykey cache.map;
      add_a1out cache ykey yval;
    end;
    if not (has_room cache) then begin
      if cache.a1out_size > cache.kout then begin
        let y = remove (tail cache.a1out) in
        cache.map <- KMap.remove y cache.map;
        cache.a1out_size <- cache.a1out_size - 1;
      end;
      (* assert (has_room cache) *)
    end

  let find_opt cache key =
    if KMap.mem key cache.map then
      Some (KMap.find key cache.map)
    else None

  let find cache key =
    match find_opt cache key with
    | Some (Amain (data, node)) ->
        cache.amain_size <- cache.amain_size - 1;
        ignore (remove node);
        add_main cache key data;
        Some data
    | Some (A1in data) ->
        Some data
    | Some (A1out (data, node)) ->
        cache.a1out_size <- cache.a1out_size - 1;
        ignore (remove node);
        add_main cache key data;
        Some data
    | None -> None

  let replace cache key data =
    match find_opt cache key with
    | Some (Amain (data',node)) ->
        if data != data' then
          cache.map <- KMap.add key (Amain (data, node)) cache.map
    | Some (A1in data') ->
        if data != data' then
          cache.map <- KMap.add key (A1in data) cache.map
    (*BISECT-IGNORE-BEGIN*)
    | Some (A1out (_,node)) ->
        cache.a1out_size <- cache.a1out_size - 1;
        ignore (remove node);
        add_main cache key data
    (*BISECT-IGNORE-END*)
    | None ->
        reclaim cache;
        add_a1in cache key data
end
