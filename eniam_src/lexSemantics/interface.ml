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

type output = Text | Xml | Html | Marsh | Graphviz
type sentence_split = Full | Partial | None

let output = ref Text
let comm_stdio = ref true
let sentence_split = ref Full
let port = ref 5439
let perform_integration = ref false
let par_names = ref false

let spec_list = [
  "-s", Arg.Unit (fun () -> sentence_split:=Full), "Split input into sentences (default)";
  "-a", Arg.Unit (fun () -> sentence_split:=Partial), "Split input into paragraphs, do not split input into sentences";
  (* "-n", Arg.Unit (fun () -> sentence_split:=None), "Do not split input into sentences"; *)
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text (default)";
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML";
  "--strong-disamb", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.strong_disambiguate_flag:=true), "Perform strong disambiguation";
  "--no-strong-disamb", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.strong_disambiguate_flag:=false), "Do not perform strong disambiguation (default)";
  "--internet-mode", Arg.Unit (fun () -> ENIAMtokenizerTypes.internet_mode:=true), "Relaxed attitude towards interpunction";
  "--no-internet-mode", Arg.Unit (fun () -> ENIAMtokenizerTypes.internet_mode:=false), "Strict attitude towards interpunction (default)";
  "--par-names", Arg.Unit (fun () -> par_names:=true), "Identifiers of paragraphs provided";
  "--no-par-names", Arg.Unit (fun () -> par_names:=false), "No identifiers of paragraphs provided (default)";
  "--proper-names", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.recognize_proper_names:=true), "Recognize proper names (default)";
  "--no-proper-names", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.recognize_proper_names:=false), "Do not recognize proper names";
  "--merge-lemmata", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.merge_lemmata:=true), "Merge lemmata (default)";
  "--no-merge-lemmata", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.merge_lemmata:=false), "Do not merge lemmata";
  "--dep-parser", Arg.Unit (fun () ->
    ENIAMpreIntegration.concraft_enabled := true;
    ENIAMpreIntegration.mate_parser_enabled := true;
    perform_integration := true), "Enable dependency parser";
  "--no-dep-parser", Arg.Unit (fun () ->
    ENIAMpreIntegration.concraft_enabled := false;
    ENIAMpreIntegration.mate_parser_enabled := false), "Disable dependency parser (default)";
  "--swigra", Arg.Unit (fun () ->
    ENIAMpreIntegration.swigra_enabled := true;
    perform_integration := true), "Enable Swigra parser";
  "--no-swigra", Arg.Unit (fun () ->
    ENIAMpreIntegration.swigra_enabled := false), "Disable Swigra parser (default)";
  "--polfie", Arg.Unit (fun () ->
    ENIAMpreIntegration.polfie_enabled := true;
    perform_integration := true), "Enable POLFIE parser";
  "--no-polfie", Arg.Unit (fun () ->
    ENIAMpreIntegration.polfie_enabled := false), "Disable POLFIE parser (default)";
  ]

let usage_msg =
  "Usage: lexSemantics <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAMlexSemantics: lexicosemantic information, MWE, abbreviation and sentence detecion for Polish\n\
Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences"

let anon_fun s = raise (Arg.Bad ("invalid argument: " ^ s))

let input_text channel =
  let s = ref (try input_line channel with End_of_file -> "") in
  let lines = ref [] in
  while !s <> "" do
    lines := !s :: !lines;
    s := try input_line channel with End_of_file -> ""
  done;
  String.concat "\n" (List.rev !lines)

let rec main_loop in_chan out_chan =
  let text = input_text in_chan in
  if text = "" then () else (
    (* print_endline "input text begin";
    print_endline text;
    print_endline "input text end"; *)
       let text,tokens,msg =
         if !sentence_split = Full then ENIAMsubsyntax.catch_parse_text true !par_names text
         else ENIAMsubsyntax.catch_parse_text false !par_names text in
       let text,msg =
         if msg <> "" || not !perform_integration then text,msg else
         ENIAMpreIntegration.catch_parse_text ENIAMsubsyntaxTypes.Struct tokens text in
       let lex_sems,msg =
         if msg <> "" then ExtArray.make 0 ENIAMlexSemanticsTypes.empty_lex_sem, msg
         else ENIAMlexSemantics.catch_assign tokens text in
       (match !output with
          Text ->
             if msg = "" then output_string out_chan (ENIAMsubsyntaxStringOf.text "" tokens text ^ "\n" ^
                    ENIAMsubsyntaxStringOf.token_extarray tokens ^ "\n\n" ^ ENIAMlexSemanticsStringOf.string_of_lex_sems tokens lex_sems ^ "\n\n")
             else output_string out_chan (ENIAMsubsyntaxStringOf.text "" tokens text ^ "\n" ^ msg ^ "\n\n")
        | Xml -> output_string out_chan (Xml.to_string (ENIAMlexSemanticsXMLof.text_and_tokens_and_lex_sems text tokens lex_sems msg) ^ "\n\n")
        | Html -> output_string out_chan (ENIAMlexSemanticsHTMLof.text_and_tokens_and_lex_sems text tokens lex_sems msg ^ "\n\n")
        | Marsh -> Marshal.to_channel out_chan (text,tokens,lex_sems,msg) []
        | Graphviz -> failwith "main_loop: ni");
    flush out_chan;
    main_loop in_chan out_chan)

let _ =
  prerr_endline message;
  Arg.parse spec_list anon_fun usage_msg;
  ENIAMlexSemantics.initialize ();
  if !perform_integration
  then ENIAMpreIntegration.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  if !comm_stdio then main_loop stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server main_loop sockaddr
