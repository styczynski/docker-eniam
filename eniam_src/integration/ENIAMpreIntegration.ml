(*
 *  ENIAMintegration, a library that integrates ENIAM with other parsers.
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>, Jan Lupa, Daniel Oklesiński
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

open ENIAMtokenizerTypes
open ENIAMsubsyntaxTypes

let concraft_enabled = ref false
let mate_parser_enabled = ref false
let swigra_enabled = ref false
let polfie_enabled = ref false

let concraft_model_filename = "../tools/concraft/nkjp-model-0.2.gz"
let concraft_server_pid = ref (-1)

let mate_parser_path = "../tools/mate-tools/"
(* let mate_model_filename = "examples/160622_Polish_MateParser.mdl" *)
let mate_model_filename = "examples/180322_PDBMate.mdl"
let mate_in = ref stdin
let mate_out = ref stdout

let swigra_path = "../tools/swigra/parser"
let swigra_in = ref stdin
let swigra_out = ref stdout
let swigra_err = ref stdin

let polfie_path = "../tools/polfie"
let polfie_results = "results/polfie"
let polfie_in = ref stdin
let polfie_out = ref stdout
let polfie_err = ref stdin

let concraft_exists () =
  let check_in, check_out, check_err = Unix.open_process_full ("command -v concraft-pl")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  let close_check () = Unix.close_process_full (check_in, check_out, check_err) in
  try
    ignore @@ input_line check_in;
    ignore @@ close_check ();
    true
  with End_of_file -> ignore @@ close_check (); false

let wait_for_concraft_server () =
  let rec wait s a =
    try Unix.connect s a
    with e -> Unix.sleep 1; wait s a in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 6 in
  let a = Unix.ADDR_INET (Unix.inet_addr_loopback, 10089) in
  wait s a;
  Unix.shutdown s Unix.SHUTDOWN_SEND;
  Unix.close s

let start_concraft_server m =
  let client_out, server_out = Unix.pipe () in
  let client_err, server_err = Unix.pipe () in
  let pid = Unix.create_process "concraft-pl" [|"concraft-pl"; "server"; "--inmodel"; m|]
    Unix.stdin server_out server_err in
  List.iter Unix.close [client_out; server_out; client_err; server_err];
  wait_for_concraft_server ();
  pid

let stop_concraft_server pid =
  Unix.kill pid Sys.sigint

let start_swigra_server dir =
  let serv_in, serv_out, serv_err = Unix.open_process_full ("cd " ^ dir ^ "; ./swigra -w")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  ignore @@ input_line serv_in;
  serv_in, serv_out, serv_err

let stop_swigra_server (serv_in, serv_out, serv_err) =
  output_string serv_out "halt.\n";
  ignore @@ Unix.close_process_full (serv_in, serv_out, serv_err)
(*
let rec read_until regexp in_channel success_msg timeout = (* FIXME: obecnie zawiesi się jeśli nie natknie się na linię matchującą do regexp *)
  let line = input_line in_channel in
  prerr_endline line;
  if Str.string_match regexp line 0
  then prerr_endline success_msg
  else read_until regexp in_channel success_msg timeout

let start_polfie_server dir =
 let serv_out = Unix.open_process_out ("cd " ^ dir ^ ";
  export XLEPATH=/opt/xle-20130507;
  export PATH=$HOME/bin:$XLEPATH/bin:/opt/morfeusz/bin:$PATH;
  export LD_LIBRARY_PATH=$XLEPATH/lib:$LD_LIBRARY_PATH;
  export DYLD_LIBRARY_PATH=$XLEPATH/lib:$DYLD_LIBRARY_PATH;
  xle -noTk") in
  serv_out

let stop_polfie_server (serv_in, serv_out, serv_err) =
  output_string serv_out "exit\n";
  ignore @@ Unix.close_process_out serv_out


let rec read_until regexp in_channel success_msg timeout = (* FIXME: obecnie zawiesi się jeśli nie natknie się na linię matchującą do regexp *)
  let line = input_line in_channel in
  prerr_endline line;
  if Str.string_match regexp line 0
  then prerr_endline success_msg
  else read_until regexp in_channel success_msg timeout

let start_polfie_server dir =
  let serv_in, serv_out, serv_err = Unix.open_process_full ("cd " ^ dir ^ ";
    export XLEPATH=/opt/xle-20130507;
    export PATH=$HOME/bin:$XLEPATH/bin:/opt/morfeusz/bin:$PATH;
    export LD_LIBRARY_PATH=$XLEPATH/lib:$LD_LIBRARY_PATH;
    export DYLD_LIBRARY_PATH=$XLEPATH/lib:$DYLD_LIBRARY_PATH;
    xle -noTk")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  ignore @@ read_until (Str.regexp_string "/home/wjaworski/Dokumenty/ENIAM3/tools/polfie/.xlerc loaded.") serv_err "Polfie ready!" 0;
  serv_in, serv_out, serv_err

let stop_polfie_server (serv_in, serv_out, serv_err) =
  output_string serv_out "exit\n";
  ignore @@ Unix.close_process_full (serv_in, serv_out, serv_err)
*)

let start_polfie_server dir = stdin, stdout, stdin
let stop_polfie_server (_,_,_) = ()

let initialize () =
  if !concraft_enabled then (
    if not (concraft_exists ()) then failwith "The command concraft-pl is missing. Please make sure Concraft is installed properly." else
    if not (Sys.file_exists concraft_model_filename) then failwith "Concraft model file does not exist." else
    print_endline "Starting Concraft Server";
    concraft_server_pid := start_concraft_server concraft_model_filename;
    print_endline "Server started");
  if !mate_parser_enabled then (
    let m_in, m_out =
      Unix.open_process ("java -jar " ^ mate_parser_path ^ "dist/anna-3.5.jar -model " ^
        mate_parser_path ^ mate_model_filename ^ " -test") in
    mate_in := m_in;
    mate_out := m_out);
  if !swigra_enabled then (
    let a,b,c = start_swigra_server swigra_path in
    swigra_in := a;
    swigra_out := b;
    swigra_err := c);
  if !polfie_enabled then (
    let a,b,c = start_polfie_server polfie_path in
    polfie_in := a;
    polfie_out := b;
    polfie_err := c)

let stop_servers () =
  if !concraft_enabled then stop_concraft_server !concraft_server_pid;
  if !swigra_enabled then stop_swigra_server (!swigra_in, !swigra_out, !swigra_err);
  if !polfie_enabled then stop_polfie_server (!polfie_in, !polfie_out, !polfie_err)

let read_whole_channel c =
  let r = ref [] in
  try
    while true do
      r := (input_line c) :: !r
    done;
    !r
  with End_of_file -> List.rev (!r)

let concraft_parse s =
  let concraft_in, concraft_out, concraft_err =
    Unix.open_process_full ("echo \"" ^ s ^ "\" | concraft-pl client")
      [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  let err_msg = String.concat "\n" (read_whole_channel concraft_err) in
  let result = read_whole_channel concraft_in in
  ignore (Unix.close_process_full (concraft_in, concraft_out, concraft_err));
  if err_msg <> "" then failwith err_msg else
  result

let rec process_concraft_result orth lemma interp others rev = function
    [] -> List.rev ((orth,(lemma,interp) :: others) :: rev)
  | "" :: l -> process_concraft_result orth lemma interp others rev l
  | line :: l ->
      (match Xstring.split_delim "\t" line with
        [orth2;s] when s = "none" || s = "space" ->
           if orth = "" then process_concraft_result orth2 lemma interp others rev l
           else process_concraft_result orth2 "" "" [] ((orth,(lemma,interp) :: others) :: rev) l
      | ["";lemma2;interp2] -> process_concraft_result orth lemma interp ((lemma2,interp2) :: others) rev l
      | ["";lemma;interp;"disamb"] -> process_concraft_result orth lemma interp others rev l
      | _ -> failwith ("process_concraft_result: " ^ line))

let make_token (orth,l) =
  if l = [] then failwith "make_token 1" else
  let lemma,interp = List.hd l in
  let cat,interp = match Xstring.split ":" interp with
      cat :: l -> cat, [Xlist.map l (fun tag -> [tag])]
    | _ -> failwith ("make_token 2: " ^ orth ^ " " ^ lemma ^ " " ^ interp) in
  {empty_token_env with orth = orth; token = Lemma(lemma,cat,interp)}

let parse_mate tokens pbeg s =
  let result = concraft_parse s in
  let l = process_concraft_result "" "" "" [] [] result in
  let l = Xlist.map l make_token in
  let l = {empty_token_env with token = Interp "<conll_root>"} :: l in
  let l = Xlist.map l (fun t -> ExtArray.add tokens t,-1,"") in
  let _ = ENIAM_CONLL.establish_lengths pbeg s l tokens in
  let dep_paths = Array.of_list l in
  (* parse_conll tokens dep_paths; *)
  dep_paths

let get_paths old_paths = function
    {sentence=DepSentence[paths]},_ ->
       Int.iter 0 (Array.length paths - 1) (fun i ->
         let id,_,_ = old_paths.(i) in
         let _,super,label = paths.(i) in
         paths.(i) <- id,super,label);
       paths
  | _ -> failwith "get_paths"

let rec parse_mate_sentence tokens pbeg s =
  let paths = parse_mate tokens pbeg s in
  print_endline "parse_mate2 1";
          (* print_endline (Visualization.html_of_dep_sentence tokens paths); *)
  let conll = ENIAM_CONLL.string_of_paths Mate tokens paths in
  print_endline "parse_mate2 2";
          (* printf "|%s|\n" conll; *)
  Printf.fprintf !mate_out "%s%!" conll;
  print_endline "parse_mate2 3";
  let new_paths = get_paths paths (ENIAM_CONLL.load_sentence !mate_in) in
  print_endline "parse_mate2 4";
          (* print_endline (Visualization.html_of_dep_sentence tokens new_paths); *)
  let paths1 = CONLL_adapter.convert_dep_tree "" true new_paths tokens in
  let paths2 = CONLL_adapter.convert_dep_tree "" false new_paths tokens in
  print_endline "parse_mate2 5";
  DepSentence[paths1;paths2]

let curl_swigra s =
  let curl_in, curl_out, curl_err = Unix.open_process_full ("curl 'http://localhost:3333/swigra' --data-urlencode 'q=" ^ s ^ "'")
    [|"PATH=" ^ Sys.getenv "PATH"; "LANG=en_GB.UTF-8"|] in
  try
    while true do
      ignore @@ input_line curl_in
    done
  with End_of_file -> ignore @@ Unix.close_process_full (curl_in, curl_out, curl_err)

(* let print_swigra_xml dir =
  let xml_in = open_in @@ dir ^ "/httpd/forest-disamb.xml" in
  try
    while true do
      print_endline @@ input_line xml_in
    done
  with End_of_file -> close_in xml_in *)


let parse_swigra_sentence tokens pbeg s =
  (* print_endline (ENIAMsubsyntaxStringOf.token_extarray tokens ^ "\n"); *)
  curl_swigra s;
  (* print_endline "s1"; *)
  let xml = Xml.parse_file (swigra_path ^ "/httpd/forest-disamb.xml") in
  (* print_endline "s2"; *)
(*  try
*)    let conll = SkladnicaXmlToConll.parse xml in
    (* print_endline "s3"; *)
    (* print_swigra_xml swigra_path; *)
    (* print_endline (conll ^ "\n"); *)
    let sentence = ENIAM_CONLL.parse_sentence conll in
    let _, conll_tokens = match sentence with
        {sentence=DepSentence[paths]}, tokens_from_conll -> paths, tokens_from_conll
      | _ -> failwith "parse_swigra_sentence" in
    (* print_endline (ENIAMsubsyntaxStringOf.token_extarray conll_tokens ^ "\n"); *)
    let l = List.rev @@ Int.fold 0 (ExtArray.size conll_tokens) [] (fun acc i ->
      ExtArray.get conll_tokens i :: acc) in
    let l = Xlist.map l (fun t -> ExtArray.add tokens t,-1,"") in
    let _ = ENIAM_CONLL.establish_lengths pbeg s l tokens in
    let paths = Array.of_list l in
    (* print_endline (ENIAMsubsyntaxStringOf.dep_sentence "" tokens paths ^ "\n"); *)
    (* print_endline (ENIAMsubsyntaxStringOf.token_extarray tokens ^ "\n"); *)
    let new_paths = get_paths paths sentence in
    (* print_endline (ENIAMsubsyntaxStringOf.dep_sentence "" tokens new_paths ^ "\n"); *)
    let paths1 = CONLL_adapter.convert_dep_tree "" true new_paths tokens in
    let paths2 = CONLL_adapter.convert_dep_tree "" false new_paths tokens in
    DepSentence[paths1;paths2]
(*  with
  | _ -> RawSentence s
*)
let parse_polfie_sentence s =
(*  let return_directory = Sys.getcwd() in
  Sys.chdir(polfie_path);
  File.file_out "sentences" (fun oc -> output_string oc s; flush oc);
  prerr_endline s;
(*  ignore @@ Sys.command("echo " ^ s ^ " > sentences; cat sentence");
  ignore @@ Sys.command("python morf2xle-Walenty.py sentence sentences morfeusz_interactive_dict"); *)
  output_string !polfie_out ("parse-testfile sentences -outputPrefix " ^ return_directory ^ "/" ^ polfie_results ^ "/fstructure-\n");
(*  output_string !polfie_out ("parse {" ^ s ^ "}\n"); *)
  flush !polfie_out;
  Sys.chdir(return_directory);
  read_until (Str.regexp_string "statistics printed on sentences.stats.") !polfie_in "Sentence parsed by POLFIE!" 0;
*)  let fstructure = File.load_file (polfie_results ^ "/fstructure-1.pl") in
 (* print_bytes fstructure; *)
  RawSentence fstructure

let compare_mode (x,_) (y,_) = compare_mode x y

let rec parse_sentence mode tokens pbeg = function
    RawSentence s ->
      [Raw,RawSentence s] @
      (if !mate_parser_enabled && !concraft_enabled then [Mate,parse_mate_sentence tokens pbeg s] else []) @
      (if !swigra_enabled then [Swigra,parse_swigra_sentence tokens pbeg s] else []) @
      (if !polfie_enabled then [POLFIE,parse_polfie_sentence s] else [])
  | StructSentence(paths,last) -> [mode,StructSentence(paths,last)]
  | DepSentence(paths) -> [mode,DepSentence paths]
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = parse_sentence mode tokens p.beg p.sentence in (* FIXME: p.pbeg czy pbeg *)
        let sentence = match sentence with
            [_,s] -> s
          | _ -> failwith "ENIAMpreIntegration.parse_sentence" in
        {p with sentence=sentence}) in
      [mode,QuotedSentences(List.rev sentences)]
  | AltSentence l ->
      let l = List.flatten (Xlist.rev_map l (fun (mode,sentence) ->
        parse_sentence mode tokens pbeg sentence)) in
      [mode,AltSentence(Xlist.sort l compare_mode)]
  | ErrorSentence s -> [mode,ErrorSentence s]

let rec parse_paragraph mode tokens = function
    RawParagraph s -> RawParagraph s
  | StructParagraph sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = parse_sentence mode tokens p.beg p.sentence in
        let sentence = match sentence with
            [_,s] -> s
          | _ -> failwith "ENIAMpreIntegration.parse_paragraph" in
        {p with sentence=sentence}) in
      StructParagraph(List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, parse_paragraph mode tokens paragraph) in
      AltParagraph(List.rev l)
  | ErrorParagraph s -> ErrorParagraph s

let rec parse_text mode tokens = function
    RawText s -> RawText s
  | StructText paragraphs ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        parse_paragraph mode tokens paragraph) in
      StructText(List.rev paragraphs)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, parse_text mode tokens text))
  | ErrorText s -> ErrorText s

let catch_parse_text mode tokens text =
  try
    parse_text mode tokens text,""
  with e ->
    text, Printexc.to_string e
