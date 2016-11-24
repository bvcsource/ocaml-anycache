#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let conf_lwt = Conf.with_pkg "lwt"
let conf_async = Conf.with_pkg "async"
let conf_coverage =
  Conf.key ~env:"BISECT_COVERAGE" "coverage" Conf.bool ~absent:false

let () =
  let cmd c os files =
    let coverage = Cmd.(on (Conf.value c conf_coverage)
                          (v "-tag" % "package(bisect_ppx)")) in
    OS.Cmd.run
      Cmd.(Pkg.build_cmd c os %% coverage % "-j" % "0" %% of_list files) in
  let build = Pkg.build ~cmd () in
  let lint_deps_excluding = Some ["bisect_ppx"] in
  let opams = [Pkg.opam_file ~lint_deps_excluding "opam"] in
  Pkg.describe "anycache" ~build ~opams @@ fun c ->
  let lwt = Conf.value c conf_lwt in
  let async = Conf.value c conf_async in
  Ok [
    (let api = ["Anycache";"Anycache_pendinglimit"] in
     Pkg.mllib ~api "src/anycache.mllib");
    Pkg.mllib ~cond:lwt "src/anycache_lwt.mllib";
    Pkg.mllib ~cond:async "src/anycache_async.mllib";
    Pkg.test ~cond:lwt "test/lwt/anycache_lwt_test";
    Pkg.test ~cond:async "test/async/anycache_async_test";
    Pkg.test "test/direct/anycache_direct_test";
    Pkg.doc "test/example.ml"
  ]
