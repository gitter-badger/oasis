(******************************************************************************)
(* OASIS: architecture for building OCaml libraries and applications          *)
(*                                                                            *)
(* Copyright (C) 2011-2013, Sylvain Le Gall                                   *)
(* Copyright (C) 2008-2011, OCamlCore SARL                                    *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

open OASISUtils
open OASISRecDescParser

let default_oasis_fn = "_oasis"

let from_lines ~ctxt ?fn lines =
  OASISAst.to_package {oasisfn = fn; ctxt = ctxt} lines

let from_file ~ctxt fn =
  IO.with_file_in fn
    (fun ic ->
       let lines = IO.read_lines ic in
       from_lines ~ctxt lines)

(** [from_string ~conf str] Parse the OASIS string [str] and check it using
    context [conf].
*)
let from_string ~ctxt ?fn str =
  let lines = OASISString.split_newline str in
  from_lines ~ctxt ?fn lines

