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

open ENIAMsubsyntaxTypes
open Xstd

let rules = ENIAM_LCGlexicon.make_rules_list false [ENIAM_LCGlexiconTypes.rules_filename; ENIAM_LCGlexiconTypes.user_lexicon_filename]
let dep_rules = ENIAM_LCGlexicon.make_rules_list true [ENIAM_LCGlexiconTypes.rules_filename; ENIAM_LCGlexiconTypes.user_lexicon_filename]

type output = Text |(* Marked |*) Xml | Html | Marsh (*| FStruct | JSON*) (*| Graphviz*)

let output = ref Html
let comm_stdio = ref true
let port = ref 5439
let lexSemantics_built_in = ref true
let lexSemantics_host = ref "localhost"
let lexSemantics_port = ref 5739
let verbosity = ref 1
let img = ref 1
let timeout = ref 30.
let select_sentence_modes_flag = ref false
let select_sentences_flag = ref true
let semantic_processing_flag = ref true
let discontinuous_parsing_flag = ref false
let correct_spelling_flag = ref false
let output_dir = ref "results/"
let perform_integration = ref false
let name_length = ref 20
let split_pattern = ref ""
let max_cost = ref 2
let spec_list = [
  "-i", Arg.Unit (fun () -> comm_stdio:=true), "Communication using stdio (default)";
  "-p", Arg.Int (fun p -> comm_stdio:=false; port:=p), "<port> Communication using sockets on given port number";
  "-t", Arg.Unit (fun () -> output:=Text), "Output as plain text";
  (* "-r", Arg.Unit (fun () -> output:=Marked), "Output as HTML marked text"; *)
  (* "-f", Arg.Unit (fun () -> output:=FStruct), "Output as feature structure"; *)
  "-x", Arg.Unit (fun () -> output:=Xml), "Output as XML";
  (* "-j", Arg.Unit (fun () -> output:=JSON), "Output as JSON"; *)
  "-m", Arg.Unit (fun () -> output:=Marsh), "Output as marshalled Ocaml data structure";
  "-h", Arg.Unit (fun () -> output:=Html), "Output as HTML (default)";
  (* "-y", Arg.Unit (fun () -> output:=Yaml), "Output as YAML"; *)
  (*"-g", Arg.Unit (fun () -> output:=Graphviz; sentence_split:=false), "Output as graphviz dot file; turns sentence split off";*)
  (* "-r", Arg.String (fun p ->
        ENIAMtokenizerTypes.set_resource_path p;
        ENIAMmorphologyTypes.set_resource_path p;
        ENIAMsubsyntaxTypes.set_resource_path p), "<path> Set resource path"; *)
  "-b", Arg.Unit (fun () -> lexSemantics_built_in:=true), "Use built in version of ENIAMlexSemantics (default)";
  "--port", Arg.Int (fun p -> lexSemantics_built_in:=false; lexSemantics_port:=p), "<port> Connect to ENIAMlexSemantics on a given port";
  "--host", Arg.String (fun s -> lexSemantics_built_in:=false; lexSemantics_host:=s), "<hostname> Connect to ENIAMlexSemantics on a given host (by default localhost)";
  "--timeout", Arg.Float (fun x -> timeout:=x), "<seconds> Sets timeout value for parser (default 30 seconds)";
  "-v", Arg.Int (fun v -> verbosity:=v), "<val> Sets verbosity level of parser\n     0 - print only status information\n     1 - print data relevant to the status of a given sentence (default)\n     2 - print all data structures";
  "--img", Arg.Int (fun v -> img:=v), "<val> Selects which images are included in output html page \n     0 - no images included\n     1 - simple dependency trees included (default)\n     2 - dependency trees included";
  "--output", Arg.String (fun s -> output_dir:=s), "<dir> Sets output directory (by default results/)";
  "--sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=true), "Select sentence modes";
  "--no-sel-modes", Arg.Unit (fun () -> select_sentence_modes_flag:=false), "Do not select sentence modes (default)";
  "--sel-sent", Arg.Unit (fun () -> select_sentences_flag:=true), "Select parsed sentences (default)";
  "--no-sel-sent", Arg.Unit (fun () -> select_sentences_flag:=false), "Do not select parsed sentences";
  "--sem", Arg.Unit (fun () -> semantic_processing_flag:=true), "Perform semantic processing (default)";
  "--no-sem", Arg.Unit (fun () -> semantic_processing_flag:=false), "Do not perform semantic processing";
  "--discontinuous", Arg.Unit (fun () -> discontinuous_parsing_flag:=true), "Parse discontinuous constituents";
  "--no-discontinuous", Arg.Unit (fun () -> discontinuous_parsing_flag:=false), "Do not parse discontinuous constituents (default)";
  "--partial", Arg.Unit (fun () -> ENIAMexecTypes.partial_parsing_flag:=true), "Build derivation trees for partially parsed sentences";
  "--no-partial", Arg.Unit (fun () -> ENIAMexecTypes.partial_parsing_flag:=false), "Build derivation trees for partially parsed sentences (default)";
  "--def-cat", Arg.Unit (fun () -> ENIAM_LCGlexiconTypes.default_category_flag:=true), "Create default semantic category for unknown tokens (default)";
  "--no-def-cat", Arg.Unit (fun () -> ENIAM_LCGlexiconTypes.default_category_flag:=false), "Do not create default semantic category for unknown tokens";
  "--internet-mode", Arg.Unit (fun () -> ENIAMtokenizerTypes.internet_mode:=true), "Relaxed attitude towards interpunction";
  "--no-internet-mode", Arg.Unit (fun () -> ENIAMtokenizerTypes.internet_mode:=false), "Strict attitude towards interpunction (default)";
  "--max-par-name-length", Arg.Int (fun v -> name_length:=v), "<val> Defines maximum length of paragraph name in visualization (default 20)";
  "--split-pattern", Arg.String (fun s -> split_pattern:=s), "<val> Defines splitting pattern for HTML marked text (default none)";
(*  "--correct-spelling", Arg.Unit (fun () -> correct_spelling_flag:=true), "Correct spelling errors before parsing";
  "--no-correct-spelling", Arg.Unit (fun () -> correct_spelling_flag:=false), "Do not correct spelling errors before parsing (default)";
*)  "--max-cost", Arg.Int (fun cost -> max_cost:=cost), "<cost> Maximal parsing cost (default 2)";
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
  "Usage: semparser <options>\nInput is a sequence of lines. Empty line ends the sequence and invoke parsing. Double empty line shutdown parser.\nOptions are:"

let message = "ENIAM_LCGparser, semantic parser for Logical Categorial Grammar formalism\n\
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

let rec main_loop sub_in sub_out in_chan out_chan =
  let text = input_text in_chan in
  if text = "" then () else (
    let text,tokens,lex_sems,msg =
      if !lexSemantics_built_in then
        let text,tokens,msg = ENIAMsubsyntax.catch_parse_text true false text in
        let text,msg =
          if msg <> "" || not !perform_integration then text,msg else
          ENIAMpreIntegration.catch_parse_text ENIAMsubsyntaxTypes.Struct tokens text in
        let lex_sems,msg =
          if msg <> "" then ExtArray.make 0 ENIAMlexSemanticsTypes.empty_lex_sem, msg
          else ENIAMlexSemantics.catch_assign tokens text in
        text,tokens,lex_sems,msg else (
      Printf.fprintf sub_out "%s\n\n%!" text;
      (Marshal.from_channel sub_in : ENIAMsubsyntaxTypes.text * ENIAMtokenizerTypes.token_env ExtArray.t * ENIAMlexSemanticsTypes.lex_sem ExtArray.t * string)) in
    if msg <> "" then
      (match !output with
      | Text (*| Marked*) -> Printf.fprintf out_chan "%s\n%!" msg
      | Html -> Printf.fprintf out_chan "%s\n%!" msg
      | Xml -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (ENIAMexecXMLof.message msg))
      (* | JSON -> Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" (JSONconverter.message msg)) *)
      | Marsh -> Marshal.to_channel out_chan (text,tokens,lex_sems,msg) []; flush out_chan) else (
    let text = ENIAMexec.translate_text text in
    let text = ENIAMexec.parse !timeout !verbosity !max_cost rules dep_rules tokens lex_sems text in
    (* let text = ENIAMdisamb.disambiguate text in
    let text = ENIAMdisamb.sort_arguments tokens text in
    let text = ENIAMdisamb.merge_mwe tokens text in *)
    let text = if !select_sentence_modes_flag then ENIAMselectSent.select_sentence_modes_text text else text in
    let text = if !select_sentences_flag then ENIAMselectSent.select_sentences_text ENIAMexecTypes.Struct text else text in
    let text = if !semantic_processing_flag then ENIAMexec.semantic_processing !verbosity tokens lex_sems text else text in
    (match !output with
    | Text -> Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (ENIAMvisualization.to_string_text !verbosity tokens text)) (* FIXME: obcinanie nazw *)
    (* | Marked -> Printf.fprintf out_chan "%s\n%!" (String.concat "\n" (ENIAMvisualization.marked_string_of_text !verbosity tokens text)) *)
    (* | Marked -> ENIAMvisualization.print_html_marked_text !output_dir "marked_text" text !img !verbosity tokens *)
    (* | Marked -> MarkedHTMLof.print_html_marked_simple_text !output_dir "marked_text" !name_length (MarkedHTMLof.marked_string_of_text !verbosity tokens text) *)
    | Html -> ENIAMvisualization.print_html_text !output_dir "parsed_text" text !img !verbosity tokens
    | Xml -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (ENIAMexecXMLof.text "" text))
    (* | FStruct -> Printf.fprintf out_chan "%s\n%!" (Xml.to_string_fmt (FStruct.text "" text)) *)
    (* | JSON ->
        (* Printf.fprintf out_chan "-------------------------\n"; *)
        (* Printf.fprintf out_chan "%s\n%!" raw_text; *)
        let json = JSONconverter.convert text in
        (* Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" json); *)
        let json2 = JSONconverter.add_text raw_text (JSONconverter.convert_jstring_indexes (JSONconverter.convert_to_kuba json)) in
        Printf.fprintf out_chan "%s\n%!" (JSONconverter.to_string "" json2);
        (* (try
          let time = TestTime.execute_gen json in
          Printf.fprintf out_chan "%s\n%!" (TestTime.string_of "" time)
         with Failure t -> print_endline ("FAILURE: " ^ t)); *)
        () *)
    | Marsh -> Marshal.to_channel out_chan (text,tokens,lex_sems,msg) []; flush out_chan));
    prerr_endline "Done!";
    main_loop sub_in sub_out in_chan out_chan)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let _ =
  prerr_endline message;
  ENIAMsemTypes.user_ontology_flag := false;
  ENIAMcategoriesPL.initialize ();
  (* ENIAMsemLexicon.initialize (); *)
  ENIAMsemLexicon.sem_lexicon := ENIAMsemLexicon.load_lexicon "data/sem-lexicon.dic";
  (* MarkedHTMLof.initialize (); *)
  Arg.parse spec_list anon_fun usage_msg;
  let application_rules = if !ENIAMtokenizerTypes.internet_mode then ENIAM_LCGrules.application_rules_ignore_brackets else ENIAM_LCGrules.application_rules in
  if !discontinuous_parsing_flag then ENIAMexecTypes.lcg_rules := application_rules @ ENIAM_LCGrules.cross_composition_rules
  else ENIAMexecTypes.lcg_rules := application_rules;
  if !lexSemantics_built_in
  then
    begin
      ENIAMlexSemantics.initialize ();
      if !perform_integration then ENIAMpreIntegration.initialize ()
    end;
  Gc.compact ();
  let sub_in,sub_out =
    if !lexSemantics_built_in then stdin,stdout
    else Unix.open_connection (get_sock_addr !lexSemantics_host !lexSemantics_port) in
  prerr_endline "Ready!";
  if !comm_stdio then main_loop sub_in sub_out stdin stdout
  else
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,!port) in
    Unix.establish_server (main_loop sub_in sub_out) sockaddr

let examples = [
  (* "Szpak","Szpak śpiewa."; *)
  (* "miał","Miałem miał."; *)
(*  "Ala","Ala ma kota.";
  "Ale","Ale mają kota:"; *)
  (*  "zima","Szpak frunie zimą.";*)
  (* "październik","Kot miauczy w październiku."; *)
(*  "Szpak-Kot","Szpak frunie. Kot miauczy.";
    "powiedział","Szpak powiedział: „Frunę. Kiszę.”";*)
    (* "teraz","Teraz frunie jakiś szpak.";
      "chłopcy","Chłopcy mają ulicę kwiatami."; *)
     (*  "arabia","Arabia Saudyjska biegnie.";*)
(*  "Tom","Tom idzie."; *)
  (* "liceum","W 1984-89 uczęszczał do VII Liceum Ogólnokształcącego im. K.K. Baczyńskiego w Szczecinie.";
  "studia","Następnie studiował architekturę na Politechnice Szczecińskiej, dyplom uzyskał w 1994."; *)
  (* "przez_nią","Frunę przez nią.";  *)
  (* "o_nie","Witold frasuje się o nie."; *)
  (* "or1","- Frunę.";  *)
  (* "or2","- Frunę - powiedział szpak."; *)
  (*"or3","- Frunę! - powiedział szpak.";*)
]
(*
let _ =
  ENIAMsubsyntax.initialize ();
  ENIAMcategoriesPL.initialize ();
  ENIAMwalParser.initialize ();
  ENIAMwalReduce.initialize ();
  Xlist.iter examples (fun (name,example) ->
  let text,tokens,msg = ENIAMsubsyntax.catch_parse_text example in
  if msg <> "" then print_endline msg else (
    let lex_sems = ENIAMlexSemantics.assign tokens text in
    let text = ENIAMexec.translate_text text in
    let text = ENIAMexec.parse 30. !verbosity rules tokens lex_sems text in
    (* let text = ENIAMselectSent.select_sentence_modes_text text in *)
    let text = ENIAMselectSent.select_sentences_text ENIAMexecTypes.Struct text in
    ENIAMvisualization.print_html_text "results/" "parsed_text" text !img !verbosity tokens))
    *)
