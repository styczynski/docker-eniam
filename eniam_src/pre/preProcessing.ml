(*
 *  ENIAM: Categorial Syntactic-Semantic Parser for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(*
let parse query =
(*   print_endline "a1"; *)
  let l = Xunicode.classified_chars_of_utf8_string query in
(*   print_endline "a2"; *)
  let l = PreTokenizer.tokenize l in
(*   print_endline "a3"; *)
  let l = PrePatterns.normalize_tokens [] l in
(*   print_endline "a4"; *)
  let l = PrePatterns.find_replacement_patterns l in
(*   print_endline "a5"; *)
  let l = PrePatterns.remove_spaces [] l in
  let l = PrePatterns.find_abr_patterns PreAcronyms.abr_patterns l in
  let l = PrePatterns.normalize_tokens [] l in
(*   print_endline "a6"; *)
  let paths = PrePaths.translate_into_paths l in
(*   print_endline "a7"; *)
  let paths = PrePaths.lemmatize paths in
(*   print_endline "a8"; *)
  let paths,_ = PreMWE.process paths in
(*   print_endline "a12"; *)
  let paths = find_proper_names paths in
(*   print_endline "a13"; *)
  let paths = modify_weights paths in
  let paths = translate_digs paths in
  let paths = assign_senses paths in
(*   print_endline "a14"; *)
  let paths = assign_valence paths in
(*   print_endline "a15"; *)
  let paths = combine_interps paths in
(*   print_endline "a16"; *)
  let paths = disambiguate_senses paths in
  let paths = assign_simplified_valence paths in
  let paths = PreSemantics.assign_semantics paths in
(*   print_endline "a16"; *)
  let paths = select_tokens paths in
(*   print_endline "a17"; *)
(*  let paths = if !single_sense_flag then single_sense paths else paths in
  let paths = if !single_frame_flag then single_frame paths else paths in*)
  (*let paths, next_id = add_ids paths next_id in
  let paths = prepare_indexes paths in*)
(*   print_endline "a18"; *)
  paths(*, next_id*)
(*     print_endline (PrePaths.to_string paths);     *)
(*   let paths =
    if PrePaths.no_possible_path (PrePaths.map paths PreLemmatization.remove_postags) then
      PrePaths.map paths process_ign
    else paths in
  let paths = PrePaths.map paths PreLemmatization.remove_postags in
  let paths = PreCaseShift.manage_lower_upper_case paths in (* FIXME: niepotrzebnie powiększa pierwszy token (przymiotniki partykuły itp.) *)
  let paths = PreLemmatization.combine_interps paths in
(*     print_endline (PrePaths.to_string paths);     *)*)

let parse_conll tokens dep_paths = (* FIXME: sprawdzić, czy zachowana jest kolejność elementów paths !!! *)
  let paths = List.rev (Int.fold 1 (Array.length dep_paths - 1) [] (fun paths conll_id ->
    let id,_,_ = dep_paths.(conll_id) in
    ExtArray.get tokens id :: paths)) in
  (* print_endline "a12"; *)
  let paths = find_proper_names paths in
  (*   print_endline "a13"; *)
  let paths = modify_weights paths in
  let paths = PreWordnet.assign_senses paths in
  (*   print_endline "a14"; *)
    (* let paths = combine_interps paths in (* FIXME: to powinno też działać dla Proper *) *)
  (*   print_endline "a15"; *)
  let paths = assign_valence paths in
  (*   print_endline "a16"; *)
  let paths = disambiguate_senses paths in
  let paths = assign_simplified_valence paths in
  let paths = PreSemantics.assign_semantics paths in
  (*   print_endline "a16"; *)
  let _ = Xlist.fold paths 1 (fun conll_id t ->
    let id,_,_ = dep_paths.(conll_id) in
    ExtArray.set tokens id t;
    conll_id + 1) in
  ()

let parse_text = function
    RawText query ->
      let text,tokens = ENIAMsubsyntax.parse_text query in
      let text = ENIAMpreIntegration.parse_text ENIAMsubsyntaxTypes.Struct tokens text in
      let lex_sems = ENIAMlexSemantics.assign tokens text in
      text,tokens,lex_sems
  | AltText[Raw,RawText query;CONLL,StructText([
            StructParagraph[{psentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence dep_paths]} as p]],tokens)] ->
        parse_conll tokens dep_paths;
        let paths = parse query in
        let sentences = PreSentences.split_into_sentences "" query tokens paths in
        let m_dep_paths = Array.map (fun (id,_,_) -> id,-1,"") dep_paths in
        let conll = StructParagraph[{p with psentence = AltSentence([Raw, RawSentence text; CONLL, DepSentence dep_paths]
          @ if Paths.config.Paths.mate_parser_enabled then [Mate, DepSentence m_dep_paths] else [])}] in
        AltText[Raw,RawText query; Struct, StructText([
          AltParagraph[Raw,RawParagraph query; ENIAM, StructParagraph sentences; CONLL, conll]],tokens)]
  | _ -> failwith "parse_text: not implemented"*)

open ENIAMsubsyntaxTypes

let parse_text = function
    RawText query,_ ->
      let text,tokens = ENIAMsubsyntax.parse_text query in
      let text = ENIAMpreIntegration.parse_text ENIAMsubsyntaxTypes.Struct tokens text in
      let lex_sems = ENIAMlexSemantics.assign tokens text in
      text,tokens,lex_sems
  | AltText[Raw,RawText query;CONLL,StructText[
            StructParagraph[{sentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence dep_paths]} as p]]],tokens ->
      let m_dep_paths = Array.map (fun (id,_,_) -> id,-1,"") dep_paths in
      let conll = StructParagraph[{p with sentence = AltSentence([Raw, RawSentence text; CONLL, DepSentence dep_paths]
        @ if Paths.config.Paths.mate_parser_enabled then [Mate, DepSentence m_dep_paths] else [])}] in
      let paths = ENIAMsubsyntax.parse query in
      let sentences = ENIAMsentences.split_into_sentences "" query tokens paths in
      let text = AltText[Raw,RawText query; Struct, StructText([
        AltParagraph[Raw,RawParagraph query; ENIAM, StructParagraph sentences; CONLL, conll]])] in
      let lex_sems = ENIAMlexSemantics.assign tokens text in
      text,tokens,lex_sems
  | _ -> failwith "parse_text: not implemented"

let rec main_loop in_chan out_chan =
  (* print_endline "main_loop 1"; *)
  let query = (Marshal.from_channel in_chan : text * ENIAMtokenizerTypes.token_env ExtArray.t) in
  (* print_endline "main_loop 2"; *)
  if fst query = RawText "" then () else (
  (try
(*     let time0 = Sys.time () in *)
    let utime0 = Unix.gettimeofday () in
   (* print_endline "main_loop 3a"; *)
    let text,tokens,lex_sems = parse_text query in
   (* print_endline "main_loop 4a"; *)
(*     let time2 = Sys.time () in *)
    let utime2 = Unix.gettimeofday () in
(*     Printf.printf "time=%f utime=%f\n%!" (time2 -. time0) (utime2 -. utime0); *)
    Marshal.to_channel out_chan (text,tokens,lex_sems,"",utime2 -. utime0) [];
  (* print_endline "main_loop 5"; *)
    ()
  with e -> (
    (* print_endline "main_loop 7"; *)
    Marshal.to_channel out_chan (
      RawText "",
      ExtArray.make 1 ENIAMtokenizerTypes.empty_token_env,
      ExtArray.make 1 ENIAMlexSemanticsTypes.empty_lex_sem,
      Printexc.to_string e,
      0.) []));
      (* print_endline "main_loop 6"; *)
  flush out_chan;
  main_loop in_chan out_chan)

(* let _ = main_loop stdin stdout *)

let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any,Paths.pre_port)

let _ =
  Gc.compact ();
  print_endline "Ready!";
  Unix.establish_server main_loop sockaddr
