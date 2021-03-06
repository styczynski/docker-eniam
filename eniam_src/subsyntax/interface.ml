(*
 *  ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
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

open ENIAMsubsyntaxTypes

let theories_paths = [
  "/home/yacheu/Dokumenty/ENIAM/theories/numbers";
  "/home/yacheu/Dokumenty/ENIAM/theories/persons";
  (* "theories/numbers"; *)
  ]

type output = Text | Xml | Html | Marsh | Graphviz | Conll
type sentence_split = Full | Partial | None

let output = ref Text
let comm_stdio = ref true
let sentence_split = ref Full
let port = ref 5439
let par_names = ref false
let output_dir = ref "results/"
let name_length = ref 20
let select_not_parsed = ref false
let sort_sentences = ref false

let spec_list = [
  "-s", Arg.Unit (fun () -> sentence_split:=Full), "Split input into sentences (default)";
  "-a", Arg.Unit (fun () -> sentence_split:=Partial), "Split input into paragraphs, do not split input into sentences";
  "-n", Arg.Unit (fun () -> sentence_split:=None), "Do not split input into sentences";
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text (default)";
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML";
  "--conll", Arg.Unit (fun () -> output:=Conll), "Output as conll";
  "-g", Arg.Unit (fun () -> output:=Graphviz; sentence_split:=None), "Output as graphviz dot file; turns sentence split off";
  "--coord-port", Arg.Int (fun p -> ENIAMsubsyntaxTypes.coord_enabled:=true; ENIAMsubsyntaxTypes.coord_port:=p), "<port> Connect to ENIAMcoordination on a given port";
  "--coord-host", Arg.String (fun s -> ENIAMsubsyntaxTypes.coord_host_name:=s), "<hostname> Connect to ENIAMcoordination on a given host (by default localhost)";
(*  "--strong-disamb", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.strong_disambiguate_flag:=true), "Perform strong disambiguation";
  "--no-strong-disamb", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.strong_disambiguate_flag:=false), "Do not perform strong disambiguation (default)";
  "--internet-mode", Arg.Unit (fun () -> ENIAMtokenizerTypes.internet_mode:=true), "Relaxed attitude towards interpunction";
  "--no-internet-mode", Arg.Unit (fun () -> ENIAMtokenizerTypes.internet_mode:=false), "Strict attitude towards interpunction (default)";*)
  "--par-names", Arg.Unit (fun () -> par_names:=true), "Identifiers of paragraphs provided";
  "--no-par-names", Arg.Unit (fun () -> par_names:=false), "No identifiers of paragraphs provided (default)";
(*  "--proper-names", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.recognize_proper_names:=true), "Recognize proper names (default)";
  "--no-proper-names", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.recognize_proper_names:=false), "Do not recognize proper names";*)
  "--merge-lemmata", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.merge_lemmata:=true), "Merge lemmata (default)";
  "--no-merge-lemmata", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.merge_lemmata:=false), "Do not merge lemmata";
  "--concraft", Arg.Unit (fun () ->
    concraft_enabled := true), "Enable Concraft tagger";
  "--no-concraft", Arg.Unit (fun () ->
    concraft_enabled := false), "Disable Concraft tagger (default)";
  "--concraft-port", Arg.Int (fun p ->
    concraft_enabled:=true;
    concraft_port:=p), "<port> Connect to Concraft tagger on a given port (by default " ^ string_of_int !concraft_port ^ ")";
  "--concraft-host", Arg.String (fun s ->
    concraft_enabled:=true;
    concraft_host_name:=s), "<hostname> Connect to Concraft tagger on a given host (by default localhost)";
  "--concraft-disambiguate", Arg.Unit (fun () ->
    concraft_disambiguate := true;
    concraft_enabled := true), "Disambiguate using Concraft tagger";
  "--no-concraft-disambiguate", Arg.Unit (fun () ->
    concraft_disambiguate := false), "Do not disambiguate using Concraft tagger (default)";
  "--max-par-name-length", Arg.Int (fun v -> name_length:=v), "<val> Defines maximum length of paragraph name in visualization (default 20)";
  "--select-not-parsed", Arg.Unit (fun () -> select_not_parsed:=true; ENIAMsubsyntaxTypes.default_category_flag:=true), "Select not parsed sentences; show default semantic category for unknown tokens";
  "--no-select-not-parsed", Arg.Unit (fun () -> select_not_parsed:=false), "Do not select not parsed sentences (default)";
  "--sort-sentences", Arg.Unit (fun () -> sort_sentences:=true), "Sort sentences";
  "--no-sort-sentences", Arg.Unit (fun () -> sort_sentences:=false), "Do not sort sentences (default)";
  "--def-cat", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.default_category_flag:=true), "Create default semantic category for unknown tokens";
  "--no-def-cat", Arg.Unit (fun () -> ENIAMsubsyntaxTypes.default_category_flag:=false; select_not_parsed:=false), "Do not create default semantic category for unknown tokens (default); do not select not parsed sentences";
  ]

let usage_msg =
  "Usage: subsyntax <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish\n\
Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>\n\
Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences"

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
    (if !sentence_split = Full || !sentence_split = Partial then
       let text,tokens(*,msg*) =
         if !sentence_split = Full then ENIAMsubsyntax.catch_parse_text true !par_names text
         else ENIAMsubsyntax.catch_parse_text false !par_names text in
       (match !output with
          Text ->
             (*if msg = "" then output_string out_chan (ENIAMsubsyntaxStringOf.text "" tokens text ^ "\n" ^
                    ENIAMsubsyntaxStringOf.token_extarray tokens ^ "\n\n")
             else*) output_string out_chan (ENIAMsubsyntaxStringOf.text "" tokens text(* ^ "\n" ^ msg*) ^ "\n\n")
        | Xml -> output_string out_chan (Xml.to_string (ENIAMsubsyntaxXMLof.text_and_tokens text tokens ""(*msg*)) ^ "\n\n")
        | Html -> output_string out_chan (ENIAMsubsyntaxHTMLof.text_and_tokens text tokens ""(*msg*) ^ "\n\n")
        | Marsh -> Marshal.to_channel out_chan (text,tokens(*,msg*)) []
        | Graphviz -> failwith "main_loop: ni"
        | Conll -> failwith "main_loop: ni")
    else
      let tokens,msg = ENIAMsubsyntax.catch_parse text in
      (match !output with
         Text ->
            if msg = "" then output_string out_chan (ENIAMsubsyntaxStringOf.token_list tokens ^ "\n\n")
            else output_string out_chan (text ^ "\n" ^ msg ^ "\n\n")
       | Xml -> output_string out_chan (Xml.to_string (ENIAMsubsyntaxXMLof.token_list tokens msg) ^ "\n\n")
       | Html -> output_string out_chan (ENIAMsubsyntaxHTMLof.token_list tokens msg ^ "\n\n")
       | Marsh -> Marshal.to_channel out_chan (tokens,msg) []
       | Conll ->
            if msg = "" then output_string out_chan (ENIAMsubsyntaxStringOf.token_list_conll tokens ^ "\n\n")
            else output_string out_chan (text ^ "\n" ^ msg ^ "\n\n")
       | Graphviz -> if msg = "" then output_string out_chan (ENIAMsubsyntaxGraphOf.token_list tokens ^ "\n\n")
            else output_string out_chan (text ^ "\n" ^ msg ^ "\n\n")));
    flush out_chan;
    main_loop in_chan out_chan)

let _ =
  prerr_endline message;
  ENIAMtokenizerTypes.theories_paths := theories_paths;
  Arg.parse spec_list anon_fun usage_msg;
  ENIAMsubsyntax.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  if !comm_stdio then main_loop stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server main_loop sockaddr
