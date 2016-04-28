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


(** Parse OASIS files using Genlex.
    @author Sylvain Le Gall
*)


open OASISTypes
open OASISAstTypes
open OASISGettext
open OASISUtils
open OASISExpr
open OASISContext
open OASISMessage
open OASISToken


(** Configuration for parsing and checking
*)
type conf =
  {
    oasisfn: host_filename option;
    ctxt:    OASISContext.t;
  }


type line_pos = int
type char_pos = int

(* one toplevel statement, either single-line or multi-line (indentation based) *)
type t =
  | RealLine of line_pos * char_pos * string (* line, begin char, data *)
  | BlockBegin
  | BlockEnd
  | StringBegin
  | StringEnd

let is_blank =
  function
    | ' ' | '\r' | '\t' -> true
    | _ -> false

let position conf lineno charno =
  Printf.sprintf
    (f_ "in file '%s' at line %d, char %d")
    (match conf.oasisfn with
      | Some fn -> fn
      | None -> "<>")
    lineno
    charno

(** Lazy list used for efficient composition *)
module L = struct
  type 'a t = 'a cell lazy_t
  and 'a cell = Nil | Cons of 'a * 'a t

  let nil = Lazy.from_val Nil
  let return x = Lazy.from_val  (Cons (x, nil))
  let cons x tl = Lazy.from_val (Cons (x, tl))
  let econs x tl = Cons (x, tl)
  let rec map f l = lazy (match l with
    | lazy Nil -> Nil
    | lazy (Cons (x,tl)) -> Cons (x,map f tl))

  let rec repeat n x = lazy (if n<=0 then Nil else Cons (x, repeat (n-1) x))

  let rec append l1 l2 = lazy (match l1 with
    | lazy Nil -> l2
    | lazy (Cons (x, l1')) -> Cons (x, append l1' l2))

  let mapi f l =
    let rec aux f l i = lazy (match l with
      | lazy Nil -> Nil
      | lazy (Cons (x, tl)) ->
        Cons (f i x, aux f tl (i+1)))
    in
    aux f l 0

  let rec fold f acc res = match res with
    | lazy Nil -> acc
    | lazy (Cons (s, cont)) -> fold f (f acc s) cont

  let rec flat_map f l = lazy (match l with
    | lazy Nil -> Nil
    | lazy (Cons (x, l')) ->
      _flat_map_app f (f x) l')
  and _flat_map_app f l l' = match l with
    | lazy Nil -> Lazy.force (flat_map f l')
    | lazy (Cons (x, tl)) ->
      Cons (x, lazy (_flat_map_app f tl l'))

  let rec filter_rec p l = match l with
    | lazy Nil -> Nil
    | lazy (Cons (x, l')) ->
      if p x
      then Cons (x, lazy (filter_rec p l'))
      else filter_rec p l'
  let filter p l = lazy (filter_rec p l)

  let flatten = flat_map (fun x->x)
  let to_list_rev l = fold (fun acc x->x::acc) [] l
  let to_list l = to_list_rev l |> List.rev

  let of_list l =
    let rec aux l = lazy (match l with
      | [] -> Nil
      | x::l' -> Cons (x, aux l'))
    in aux l
end

(* apply a series of transformations on a [t L.t]
   @param ops list of operations to perform *)
let apply_transformations (conf:conf) ~ops l =
  let debug fmt = debug ~ctxt:conf.ctxt fmt in
  List.fold_left
    (fun lines (msg, f) ->
       debug "%s:" msg;
       f lines)
    l
    ops

(* create [abs n] blocks, End if [n<0], Begin otherwise*)
let mk_blocks n =
  if n<0 then L.repeat (abs n) BlockEnd else L.repeat n BlockBegin

(* based on indentation, add begin/end block *)
let add_indent_blocks (conf:conf) lines =
  let warning fmt = warning ~ctxt:conf.ctxt fmt in
  (* recursive traversal of list *)
  let rec process prv_indent_level only_tab l = match l with
    | lazy L.Nil ->
      (* Return back to indent level 0 *)
      Lazy.force (mk_blocks (-prv_indent_level))
    | lazy (L.Cons (RealLine (lineno, charstart, str), tail)) ->
      let cur_indent_level, use_tab, use_space =
        let pos = ref 0 in
        let use_tab = ref false in
        let use_space = ref false in
        while !pos < String.length str &&
              is_blank str.[!pos] do
          use_tab   := str.[!pos] = '\t' || !use_tab;
          use_space := (not (str.[!pos] = '\t') || !use_space);
          incr pos
        done;
        !pos, !use_tab, !use_space
      in
      (* check consistency in case the user uses only tabs *)
      let only_tab =
        if use_space && use_tab then
          begin
            warning
              (f_ "Mixed use of '\\t' and ' ' to indent \
                   lines %s")
              (position conf lineno charstart);
            only_tab
          end
        else
          begin match only_tab with
            | Some use_tab_before ->
              if use_tab_before && not use_tab then
                warning
                  (f_ "Use of ' ' but '\\t' was used \
                       before to indent lines %s")
                  (position conf lineno charstart);
              if not use_tab_before && use_tab then
                warning
                  (f_ "Use of '\\t' but ' ' was used \
                       before to indent lines %s")
                  (position conf lineno charstart);

              only_tab
            | None ->
              Some use_tab
          end
      in
      let charstart = charstart + cur_indent_level in
      let str =
        String.sub
          str
          cur_indent_level
          ((String.length str) - cur_indent_level)
      in
      let diff_indent_level = cur_indent_level - prv_indent_level in
      Lazy.force
        (L.flatten
          (L.of_list
             [ mk_blocks diff_indent_level
             ; L.return (RealLine (lineno, charstart, str))
             ; lazy (process cur_indent_level only_tab tail)
             ]))
    | lazy (L.Cons (BlockBegin as e, tail)) ->
      L.econs e (lazy (process (prv_indent_level+1) only_tab tail))
    | lazy (L.Cons (BlockEnd as e, tail)) ->
      L.econs e (lazy (process (prv_indent_level-1) only_tab tail))
    | lazy (L.Cons ((StringBegin | StringEnd) as e, tail)) ->
      L.econs e (lazy (process prv_indent_level only_tab tail))
  in
  lazy (process 0 None lines)

(* split values into lists, handle multi-line values *)
let split_values lines =
  (* find next field *)
  let rec find_field l = match l with
    | lazy L.Nil -> L.Nil
    | lazy (L.Cons (RealLine (lineno, charstart, str) as e, tl)) ->
      begin
        try
          let colon_pos = String.index str ':' in
          let pos = ref (colon_pos + 1) in
          while !pos < String.length str && is_blank str.[!pos] do
            incr pos
          done;
          let line_begin =
            RealLine (lineno, charstart,
              String.sub str 0 (colon_pos + 1))
          in
          if !pos < String.length str
          then
            (* Split end of line *)
            let line_end =
              RealLine
                (lineno,
                 charstart + !pos,
                 String.sub str !pos (String.length str - !pos))
            in
            L.econs
              line_begin
              (L.cons StringBegin
                 (L.cons line_end (lazy (fetch_multiline 0 tl))))
          else
            L.econs line_begin
              (L.cons StringBegin (lazy (fetch_multiline 0 tl)))
        with Not_found -> L.econs e (lazy (find_field tl))
      end
    | lazy (L.Cons (e, tl)) -> L.econs e (lazy (find_field tl))

  (* find all lines in a multiline field *)
  and fetch_multiline lvl l =
    let lvl_ref =
      let rec count_block_begin lvl l = match l with
        | lazy (L.Cons (BlockBegin, tl)) ->
            (* Count the initial indentation (first line) *)
            count_block_begin (lvl + 1) tl
        | _ -> lvl
      in
      count_block_begin lvl l
    in

    let rec fetch_multiline_nxt lvl l = match l with
      | lazy (L.Cons (BlockBegin, tl)) ->
        fetch_multiline_nxt (lvl + 1) tl
      | lazy (L.Cons (BlockEnd, tl)) ->
        if lvl > 1 then
          fetch_multiline_nxt (lvl - 1) tl
        else if lvl = 1 then
          L.econs StringEnd (lazy (find_field tl))
        else
          L.econs StringEnd (lazy (find_field (L.cons BlockEnd tl)))
      | lazy (L.Cons (RealLine (lineno, charstart, str) as e, tl)) ->
        if lvl > 0 then
          let diff = lvl - lvl_ref in
          if diff < 0
          then failwithf
              (f_ "Unexpected indentation line %d.")
              lineno;
          L.econs
            (RealLine
               (lineno,
                charstart - diff,
                String.make diff ' ' ^ str))
            (lazy (fetch_multiline_nxt lvl tl))
        else
          L.econs StringEnd (lazy (find_field (L.cons e tl)))
      | lazy (L.Cons (StringBegin as e, tl))
      | lazy (L.Cons (StringEnd as e, tl)) ->
        if lvl > 0 then
          fetch_multiline_nxt lvl tl
        else
          L.econs StringEnd (lazy (find_field (L.cons e tl)))
      | lazy L.Nil ->
        L.econs StringEnd (lazy (find_field L.nil))
    in
    fetch_multiline_nxt lvl l
  in
  lazy (find_field lines)

let replace_dot l =
  let rec replace_dot l = match l with
    | lazy (L.Cons (RealLine (l1, _, _) as e1,
        lazy (L.Cons (RealLine (l2, c, "."),
            lazy (L.Cons (RealLine (l3, _, _) as e2, tl))))))
      when l1 <> l2 && l2 <> l3 ->
      L.econs e1
        (L.cons (RealLine (l2, c, ""))
           (lazy (replace_dot (L.cons e2 tl))))
    | lazy (L.Cons (RealLine (l1, _, _) as e1,
        lazy (L.Cons (RealLine (l2, c, "."),
            lazy (L.Cons (StringEnd, tl)))))) when l1 <> l2 ->
      L.econs e1
        (L.cons (RealLine (l2, c, ""))
           (L.cons StringEnd (lazy (lookup_string tl))))
    | lazy (L.Cons (e, tl)) ->
      L.econs e (lazy (replace_dot tl))
    | lazy L.Nil -> L.Nil

   and lookup_string =
     function
       | lazy (L.Cons (StringBegin, tl)) ->
         L.econs StringBegin (lazy (replace_dot tl))
       | lazy (L.Cons (e, tl)) ->
         L.econs e (lazy (lookup_string tl))
       | lazy L.Nil -> L.Nil
   in
   lazy (lookup_string l)

let add_eol l =
  let rec concat_string = function
    | lazy (L.Cons (RealLine (l1, charstart, str),
        (lazy (L.Cons (RealLine (l2, _, _), _)) as tl))) when l2 <> l1 ->
      L.econs
        (RealLine (l1, charstart, str^"\n"))
        (lazy (concat_string tl))
    | lazy (L.Cons (StringEnd, tl)) ->
      L.econs StringEnd (lazy (lookup_string tl))
    | lazy (L.Cons (hd, tl)) ->
      L.econs hd (lazy (concat_string tl))
    | lazy L.Nil -> L.Nil
  and lookup_string = function
    | lazy (L.Cons (StringBegin, tl)) ->
      L.econs StringBegin (lazy (concat_string tl))
    | lazy (L.Cons (e, tl)) ->
      L.econs e (lazy (lookup_string tl))
    | lazy L.Nil -> L.Nil
  in
  lazy (lookup_string l)

let parse_lines (conf:conf) (lines:t L.t) =
  apply_transformations conf lines
    ~ops:[
      "Get rid of MS-DOS file format",
      L.map
        (function
          | RealLine (lineno, charstart, str) ->
            begin
              let len =
                String.length str
              in
              let str =
                if len > 0 && str.[len - 1] = '\r' then
                  String.sub str 0 (len - 1)
                else
                  str
              in
              RealLine (lineno, charstart, str)
            end
          | e -> e);

      "Remove comments",
      L.map
        (function
          | RealLine (lineno, charstart, str) ->
            begin
              try
                let idx =
                  String.index str '#'
                in
                RealLine (lineno, charstart, String.sub str 0 idx)
              with Not_found ->
                RealLine(lineno, charstart, str)
            end
          | e -> e);

      "Remove trailing whitespaces",
      L.map
        (function
          | RealLine (lineno, charstart, str) ->
            begin
              let pos =
                ref ((String.length str) - 1)
              in
              while !pos > 0 && is_blank str.[!pos] do
                decr pos
              done;
              RealLine(lineno, charstart, String.sub str 0 (!pos + 1))
            end
          | e -> e);

      "Remove empty lines",
      L.filter
        (function
          | RealLine (_, _, "") -> false
          | _ -> true);

      "Remove leading whitespaces and replace them with begin/end block",
      add_indent_blocks conf;

      "Split values and detect multi-line values",
      split_values;

      "Replace line containing '.' only by blank line",
      replace_dot;

      "Add EOL",
      add_eol;
    ]

(* get a lazy list of tokens from the lines *)
let tokens_of_lines lines =

  (* Lexer for OASIS language *)
  let next_tok() = OASISLexer.token lexbuf in

  let position, st =
    let lineno = ref 0 in
    let charno = ref 0 in
    let virtual_pos = ref None in
    let lines_q =
      let q = Queue.create () in
      List.iter
        (fun s -> Queue.add s q)
        lines;
      q
    in
    let chars_q = Queue.create () in
    let in_string = ref false in
    let rec getc () =
      try
        incr charno;
        Some (Queue.take chars_q)
      with Queue.Empty ->
        begin
          (* Refill the char queue *)
          try
            let () =
              match Queue.take lines_q with
                | RealLine (nlineno, ncharno, str) ->
                  lineno := nlineno;
                  charno := ncharno;
                  String.iter
                    (fun c -> Queue.add c chars_q)
                    (if !in_string then
                       String.escaped str
                     else
                       str);
                | BlockBegin ->
                  virtual_pos := Some "block begin";
                  Queue.add '{'chars_q
                | BlockEnd ->
                  virtual_pos := Some "block end";
                  Queue.add '}' chars_q
                | StringBegin | StringEnd ->
                  virtual_pos := Some "string marker";
                  in_string := not !in_string;
                  Queue.add '"' chars_q
            in
            getc ()

          with Queue.Empty ->
            (* lines and char queue are empty -> nothing left *)
            None
        end
    in

    (fun fmt -> position !lineno !charno),
    Stream.from (fun _ -> getc ())
  in


let parse_lines (conf:conf) lines =
  let lines =
    L.mapi
      (fun i line -> RealLine (i, 0, line))
      (L.of_list lines)
  in
  (* preprocess the lines *)
  let lines = parse_lines conf lines in
  (* OASIS expression *)
  let rec parse_factor =
    parser
  | [< 'Kwd "true" >] ->
    EBool true
  | [< 'Kwd "True" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "TRUE" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "false" >] ->
    EBool false
  | [< 'Kwd "False" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "FALSE" >] ->
    raise (Failure "Boolean values must be lowercase.")
  | [< 'Kwd "!"; e = parse_factor >] ->
    ENot e
  | [< 'Kwd "("; e = parse_expr; 'Kwd ")" >] ->
    e
  | [< 'Ident nm; 'Kwd "("; 'Ident vl; 'Kwd ")" >] ->
    if nm = "flag" then
      EFlag vl
    else
      ETest (OASISExpr.test_of_string nm, vl)
  and parse_term_follow =
    parser
  | [< 'Kwd "&&"; e1 = parse_factor; e2 = parse_term_follow >] ->
    EAnd (e1, e2)
  | [< >] ->
    EBool true
  and parse_term =
    parser
  | [< e1 = parse_factor; e2 = parse_term_follow >] ->
    EAnd(e1, e2)

  and parse_expr_follow =
    parser
  | [< 'Kwd "||"; e1 = parse_term; e2 = parse_expr_follow >] ->
    EOr (e1, e2)
  | [< >] ->
    EBool false
  and parse_expr =
    parser
  | [< e1 = parse_term; e2 = parse_expr_follow >] ->
    EOr (e1, e2)
  in

  (* OASIS fields and flags *)
  let rec parse_else =
    parser
  | [< 'Kwd "else"; else_blk = parse_stmt>] ->
    else_blk
  | [< >] ->
    SBlock []

  and parse_field_op =
    parser
  | [<'Kwd ":"; 'String str>] ->
    FSet str

  | [<'Kwd "+:"; 'String str>] ->
    FAdd str

  | [<'Kwd "$:"; 'String str>] ->
    let e =
      try
        parse_expr (lexer (Stream.of_string str))
      with e ->
        failwithf
          (f_ "Error when parsing expression '%s' %t: %s")
          str
          position
          (Printexc.to_string e)
    in
    FEval e

  and parse_stmt =
    parser
  | [<'Ident nm; op = parse_field_op>] ->
    SField(nm, op)

  | [< 'Kwd "if"; e = parse_expr;
       if_blk = parse_stmt;
       else_blk = parse_else>] ->
    SIfThenElse(e, if_blk, else_blk)

  | [< 'Kwd "{"; lst = parse_stmt_list; 'Kwd "}">] ->
    SBlock lst

  and parse_stmt_list =
    parser
  | [< stmt = parse_stmt; tl = parse_stmt_list>] ->
    stmt :: tl
  | [< >] ->
    []
  in

  let id_or_string =
    parser
  | [<'Ident nm>] ->
    nm
  | [<'String nm>] ->
    nm
  in

  let rec parse_top_stmt =
    parser
  | [<'Kwd "Flag"; nm = id_or_string; flag_blk = parse_stmt>] ->
    TSFlag(nm, flag_blk)

  | [<'Kwd "Library"; nm = id_or_string; library_blk = parse_stmt>] ->
    TSLibrary (nm, library_blk)

  | [<'Kwd "Object"; nm = id_or_string; object_blk = parse_stmt>] ->
    TSObject (nm, object_blk)

  | [< 'Kwd "Executable"; nm = id_or_string; exec_blk = parse_stmt>] ->
    TSExecutable (nm, exec_blk)

  | [< 'Kwd "SourceRepository"; nm = id_or_string;
       src_repo_blk = parse_stmt>] ->
    TSSourceRepository (nm, src_repo_blk)

  | [< 'Kwd "Test"; nm = id_or_string; test_blk = parse_stmt>] ->
    TSTest (nm, test_blk)

  | [< 'Kwd "Document"; nm = id_or_string; test_blk = parse_stmt>] ->
    TSDocument (nm, test_blk)

  | [< 'Kwd "{"; lst = parse_top_stmt_list; 'Kwd "}">] ->
    TSBlock lst

  | [< stmt = parse_stmt >] ->
    TSStmt stmt

  and parse_top_stmt_list =
    parser
  | [< top_stmt = parse_top_stmt; tl = parse_top_stmt_list>] ->
    top_stmt :: tl
  | [< >] ->
    []
  in
  (* Main loop *)
  try
    let st_token =
      lexer st
    in
    let ast =
      TSBlock (parse_top_stmt_list st_token)
    in
    ast
  with
    | Stream.Error "" ->
      failwithf (f_ "Syntax error %t") position
    | Stream.Error str ->
      failwithf (f_ "Syntax error %s %t") str position
