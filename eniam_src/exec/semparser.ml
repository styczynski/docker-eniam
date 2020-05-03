(*
 *  ENIAMexec implements ENIAM processing stream
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

open Xstd
open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes

let rules = ENIAM_LCGlexicon.make_rules false ENIAM_LCGlexiconTypes.user_lexicon_filename
let dep_rules = ENIAM_LCGlexicon.make_rules true ENIAM_LCGlexiconTypes.user_lexicon_filename

let load_cats_map filename =
  File.fold_tab filename StringMap.empty (fun map -> function
    [lemma;cat] -> StringMap.add_inc map lemma [cat] (fun l -> cat :: l)
  | l -> failwith ("load_cats_map: " ^ String.concat "\t" l))

let cats_map = load_cats_map ENIAM_LCGlexiconTypes.user_cats_filename
let coerced_map = load_cats_map ENIAM_LCGlexiconTypes.user_coerced_filename

let subsyntax_built_in = ref true
let subsyntax_host = ref "localhost"
let subsyntax_port = ref 5739
let verbosity = ref 1
let img = ref 1
let timeout = ref 30.
let select_sentence_modes_flag = ref false
let select_sentences_flag = ref true
let semantic_processing_flag = ref true
let output_dir = ref "results/"

let spec_list = [
(*  "-s", Arg.Unit (fun () -> sentence_split:=true), "Split input into sentences (default)";
  "-n", Arg.Unit (fun () -> sentence_split:=false), "Do not split input into sentences";
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text (default)";
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML";
  "-g", Arg.Unit (fun () -> output:=Graphviz; sentence_split:=false), "Output as graphviz dot file; turns sentence split off";*)
  (* "-r", Arg.String (fun p ->
        ENIAMtokenizerTypes.set_resource_path p;
        ENIAMmorphologyTypes.set_resource_path p;
        ENIAMsubsyntaxTypes.set_resource_path p), "<path> Set resource path"; *)
  "-b", Arg.Unit (fun () -> subsyntax_built_in:=true), "Use built in version of ENIAMsubsyntax (default)";
  "--port", Arg.Int (fun p -> subsyntax_built_in:=false; subsyntax_port:=p), "<port> Connect to ENIAMsubsyntax on a given port";
  "--host", Arg.String (fun s -> subsyntax_built_in:=false; subsyntax_host:=s), "<hostname> Connect to ENIAMsubsyntax on a given host (by default localhost)";
  "--timeout", Arg.Float (fun x -> timeout:=x), "<seconds> Sets timeout value for parser (default 30 seconds)";
  "-v", Arg.Int (fun v -> verbosity:=v), "<val> Sets verbosity level of parser\n     0 - print only status information\n     1 - print data relevant to the status of a given sentence (default)\n     2 - print all data structures";
  "--img", Arg.Int (fun v -> img:=v), "<val> Selects which images are included in output html page \n     0 - no images included\n     1 - simple dependency trees included (default)\n     2 - dependency trees included";
  "--output", Arg.String (fun s -> output_dir:=s), "<dir> Sets output directory (by default results/)";
  "--sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=true), "Select sencence modes";
  "--no-sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=false), "Do not select sencence modes (default)";
  "--sel_sent", Arg.Unit (fun () -> select_sentences_flag:=true), "Select parsed sentences (default)";
  "--no-sel-sent", Arg.Unit (fun () -> select_sentences_flag:=false), "Do not select parsed sentences";
  "--sem", Arg.Unit (fun () -> semantic_processing_flag:=true), "Perform semantic processing (default)";
  "--no-sem", Arg.Unit (fun () -> semantic_processing_flag:=false), "Do not perform semantic processing";
  ]

let usage_msg =
  "Usage: semparser <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAM_LCGsemparser, semantic parser for Logical Categorial Grammar formalism\n\
Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let input_text channel =
  let s = ref (try input_line channel with End_of_file -> "") in
  let lines = ref [] in
  while !s <> "" do
    lines := !s :: !lines;
    s := try input_line channel with End_of_file -> ""
  done;
  String.concat "\n" (List.rev !lines)

let get_cats cats_map = function
    Interp orth -> (try StringMap.find cats_map orth with Not_found -> ["X"])
  | Lemma(lemma,_,_) -> (try StringMap.find cats_map lemma with Not_found -> ["X"])
  | Proper(_,_,_,cats) -> if cats = [] then ["X"] else cats
  | _ -> ["X"]

let expand_coercions coerced_map cats =
  Xlist.rev_map cats (fun cat -> cat, cat :: (try StringMap.find coerced_map cat with Not_found -> []))
(*  StringSet.to_list (Xlist.fold cats StringSet.empty (fun set cat ->
    let cats = try StringMap.find coerced_map cat with Not_found -> [] in
    Xlist.fold (cat :: cats) set StringSet.add))*)

let assign_lex_sems coerced_map cats_map tokens =
  let lex_sems = ExtArray.make (ExtArray.size tokens) ENIAMlexSemanticsTypes.empty_lex_sem in
  let _ = ExtArray.add lex_sems ENIAMlexSemanticsTypes.empty_lex_sem in
  Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
    let lemma = ENIAMtokens.get_lemma (ExtArray.get tokens i).token in
    let pos = ENIAMtokens.get_pos (ExtArray.get tokens i).token in
    let cats = expand_coercions coerced_map (get_cats cats_map (ExtArray.get tokens i).token) in
    let frames =
        Xlist.rev_map (ENIAMvalence.get_aroles [] lemma pos) (fun (sel,arole,arole_attr,arev) ->
          {ENIAMlexSemanticsTypes.empty_frame with ENIAMlexSemanticsTypes.selectors=sel; ENIAMlexSemanticsTypes.arole=arole; ENIAMlexSemanticsTypes.arole_attr=arole_attr; ENIAMlexSemanticsTypes.arev=arev}) in
    let lex_sem = {ENIAMlexSemanticsTypes.empty_lex_sem with ENIAMlexSemanticsTypes.cats=cats; ENIAMlexSemanticsTypes.frames=frames} in
    let _ = ExtArray.add lex_sems lex_sem in
    ());
  lex_sems

let rec main_loop sub_in sub_out =
  let text = input_text stdin in
  if text = "" then () else (
    let text,tokens,msg =
      if !subsyntax_built_in then ENIAMsubsyntax.catch_parse_text text else (
      Printf.fprintf sub_out "%s\n\n%!" text;
      (Marshal.from_channel sub_in : ENIAMsubsyntaxTypes.text * token_env ExtArray.t * string)) in
    if msg <> "" then print_endline msg else (
    let lex_sems = assign_lex_sems coerced_map cats_map tokens in
    let text = ENIAMexec.translate_text text in
    (* let text = ENIAMexec.parse !timeout !verbosity rules tokens lex_sems text in *)
    let text = ENIAMexec.parse !timeout !verbosity rules dep_rules tokens lex_sems text in
    let text = if !select_sentence_modes_flag then ENIAMselectSent.select_sentence_modes_text text else text in
    let text = if !select_sentences_flag then ENIAMselectSent.select_sentences_text ENIAMexecTypes.Struct text else text in
    let text = if !semantic_processing_flag then ENIAMexec.semantic_processing !verbosity tokens lex_sems text else text in
    ENIAMvisualization.print_html_text !output_dir "parsed_text" text !img !verbosity tokens);
    prerr_endline "Done!";
    main_loop sub_in sub_out)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  prerr_endline message;
  ENIAMcategoriesPL.initialize ();
  Arg.parse spec_list anon_fun usage_msg;
  if !subsyntax_built_in then ENIAMsubsyntax.initialize ();
  Gc.compact ();
  let sub_in,sub_out =
    if !subsyntax_built_in then stdin,stdout
    else Unix.open_connection (get_sock_addr !subsyntax_host !subsyntax_port) in
  prerr_endline "Ready!";
  main_loop sub_in sub_out
