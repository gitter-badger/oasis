
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

(** {1 Tokens for the Parser} *)

type t =
  | COLON
  | COLON_PLUS
  | COLON_DOLLAR
  | INDENT of int
  | IF
  | ELSE
  | LBRACE
  | RBRACE
  | FLAG
  | LIBRARY
  | EXECUTABLE
  | TEST
  | SOURCE_REPO
  | DOCUMENT
  | OBJECT
  | OR
  | NOT
  | AND
  | LPAREN
  | RPAREN
  | TRUE
  | FALSE
  | IDENT of string
  | EOF
