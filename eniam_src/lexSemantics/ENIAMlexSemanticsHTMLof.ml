(*
 *  ENIAMlexSemantics is a library that assigns tokens with lexicosemantic information.
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
 *
 *  This library is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open ENIAMlexSemanticsTypes
open Printf

let html_header =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>ENIAM: Kategorialny Parser Składniowo-Semantyczny</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_trailer =
"</center>
  </body>
</html>"

(*let html_of_lex_sems tokens lex_sems =
  "<table><td><b>id</b></td><tr><td><b>orth</b></td><td><b>token</b></td></tr>" ^
  String.concat "\n" (List.rev (Int.fold 0 (ExtArray.size tokens - 1) [] (fun l id ->
    let t = ExtArray.get tokens id in
    let s = ExtArray.get lex_sems id in
    ((sprintf "<tr><td>%d</td><td>%s</td><td>%s</td>"
      id t.ENIAMtokenizerTypes.orth (ENIAMsubsyntaxHTMLof.escape_html (ENIAMtokens.string_of_token t.ENIAMtokenizerTypes.token))) ^
    (String.concat "</tr>\n<tr><td></td><td></td><td></td>" (Xlist.map s.schemata (fun (selectors,positions) ->
      sprintf "<td>%s</td><td>%s</td>" (ENIAMcategoriesPL.string_of_selectors selectors)
        (String.concat ", " (Xlist.map positions (fun (d,t) -> ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 t)))))) ^
    "</tr>") :: l))) ^
  "</table>"*)

let html_of_lex_sems tokens lex_sems =
  "<P align=\"left\">" ^
  String.concat "<br>\n" (List.rev (Int.fold 0 (ExtArray.size lex_sems - 1) [] (fun l id ->
    let t = ExtArray.get lex_sems id in
    let t2 = ExtArray.get tokens id in
    let orth = t2.ENIAMtokenizerTypes.orth in
    let lemma = ENIAMtokens.string_of_token t2.ENIAMtokenizerTypes.token in
    let core = Printf.sprintf "%3d %s %s" id orth lemma  in
    let lex_entries = Xlist.map t.lex_entries (fun (selectors,s) ->
        "&emsp;&emsp;[" ^ ENIAMcategoriesPL.string_of_selectors selectors ^ "] " ^ ENIAM_LCGstringOf.grammar_symbol 0 s) in
    let schemata = Xlist.map t.schemata (fun (selectors,cat,(*snode,*)l,l2,l3) ->
        "&emsp;&emsp;[" ^ ENIAMcategoriesPL.string_of_selectors selectors ^ "]" ^
        String.concat "," (Xlist.map cat (fun (m,l) -> m ^ "[" ^ String.concat "," l ^ "]")) ^
        (*String.concat "|" snode ^*)
        " {" ^ String.concat ", " (Xlist.map l (fun (d,s) ->
            ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 s)) ^ "}" ^
        " {" ^ String.concat ", " (Xlist.map l2 (fun (d,s) ->
            ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 s)) ^ "}" ^
        " {" ^ String.concat ", " (Xlist.map l3 (fun (d,s) ->
            ENIAM_LCGstringOf.direction d ^ ENIAM_LCGstringOf.grammar_symbol 0 s)) ^ "}") in
    (* let frames = Xlist.map t.frames (fun (selectors,senses,schema) -> FIXME
        "&emsp;&emsp;[" ^ ENIAMcategoriesPL.string_of_selectors selectors ^ "] {" ^ ENIAMwalStringOf.schema schema ^ "} " ^
        String.concat ", " (Xlist.map senses (fun m -> ENIAMwalStringOf.sense m))) in *)
    (String.concat "<br>\n    " ([core] @ schemata (*@ frames*) @ lex_entries)) :: l))) ^
  "</P>"

(*  schemata: ((ENIAM_LCGlexiconTypes.selector * ENIAM_LCGlexiconTypes.selector_relation * string list) list *
             (ENIAM_LCGtypes.direction * ENIAM_LCGtypes.grammar_symbol) list) list;
  lex_entries: ((ENIAM_LCGlexiconTypes.selector * ENIAM_LCGlexiconTypes.selector_relation * string list) list *
                ENIAM_LCGtypes.grammar_symbol) list;
  frames: ((ENIAM_LCGlexiconTypes.selector * ENIAM_LCGlexiconTypes.selector_relation * string list) list *
             ENIAMwalTypes.sense list * ENIAMwalTypes.position list) list;*)

let text_and_tokens_and_lex_sems text tokens lex_sems msg =
  if msg = "" then sprintf "%s\n%s<BR>\n%s<BR>\n%s<BR>\n%s\n" html_header
   (ENIAMsubsyntaxHTMLof.html_of_text tokens text) (ENIAMsubsyntaxHTMLof.html_of_token_extarray tokens)
   (html_of_lex_sems tokens lex_sems) html_trailer
  else sprintf "%s\n%s\n%s\n" html_header msg html_trailer

let print_text_and_tokens_and_lex_sems path name text tokens lex_sems msg =
  File.file_out (path ^ name ^ ".html") (fun file ->
      output_string file (text_and_tokens_and_lex_sems text tokens lex_sems msg ))
