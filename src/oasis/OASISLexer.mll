(********************************************************************************)
(*  OASIS: architecture for building OCaml libraries and applications           *)
(*                                                                              *)
(*  Copyright (C) 2011-2013, Sylvain Le Gall                                    *)
(*  Copyright (C) 2008-2011, OCamlCore SARL                                     *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)

{
open OASISToken

let get_loc lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let s_l = start.Lexing.pos_lnum in
  let s_c = start.Lexing.pos_cnum - start.Lexing.pos_bol in
  s_l, s_c

let loc_to_str (l,c) =
  Printf.sprintf "line %d, col %d" l c

let fail_ lexbuf msg =
  let pos = get_loc lexbuf in
  let msg = Printf.sprintf "at %s: %s" (loc_to_str pos) msg in
  failwith msg
}

rule token = parse
[' ' '\t']       {token lexbuf}
| '\n'           {Lexing.new_line lexbuf; token lexbuf}
| ":"            {COLON}
| "+:"           {COLON_PLUS}
| "+$"           {COLON_DOLLAR}
| "if"           {IF}
| "else"         {ELSE}
| "{"            {RBRACE}
| "}"            {LBRACE}
| "Library"      {LIBRARY}
| "Executable"   {EXECUTABLE}
| "Object"       {OBJECT}
| "SourceRepository"  {SOURCE_REPO}
| "Test"         {TEST}
| "Document"     {DOCUMENT}
| "Flag"         {FLAG}
| "||"           {OR}
| "&&"           {AND}
| '!'            {NOT}
| '('            {LPAREN}
| ')'            {RPAREN}
| "true"         {TRUE}
| "True"         {TRUE}
| "false"        {FALSE}
| "False"        {FALSE}
| "TRUE"
| "FALSE"        {fail_ lexbuf "boolean are not capitalized"}
| eof            {EOF}
| ['A'-'Z''a'-'z''0'-'9''-''_']+ as lxm {IDENT(lxm)}
| _              {fail_ lexbuf "lexing error"}

