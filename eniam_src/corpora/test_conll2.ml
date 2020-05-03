(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
 *  Copyright (C) 2016 Daniel Oklesinski <oklesinski dot daniel atSPAMfree gmail dot com>
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

open Xstd
(*open ENIAM_LCGlexiconTypes
open ENIAM_LCGtypes
open ENIAMsubsyntaxTypes

(* let parsed = ref 1 *)

let rules = ENIAM_LCGlexicon.make_rules false ENIAM_LCGlexiconTypes.rules_filename
let dep_rules = ENIAM_LCGlexicon.make_rules true ENIAM_LCGlexiconTypes.rules_filename

let examples = [
  (* "Szpak","Szpak śpiewa.";*)
  (* "miał","Miałem miał."; *)
  (*  "Ala","Ala ma kota.";
      "Ale","Ale mają kota:"; *)
  (*  "zima","Szpak frunie zimą.";*)
  (* "październik","Kot miauczy w październiku."; *)
  (*  "Szpak-Kot","Szpak frunie. Kot miauczy.";
      "powiedział","Szpak powiedział: „Frunę. Kiszę.”";*)
  "teraz","Teraz frunie jakiś szpak.";
  "chłopcy","Chłopcy mają ulicę kwiatami.";
  (*  "arabia","Arabia Saudyjska biegnie.";*)
  (*  "Tom","Tom idzie."; *)
]

(* let clarify_categories senses token =
  match token.ENIAMtokenizerTypes.token with
    ENIAMtokenizerTypes.Lemma(lemma,pos,interp) -> List.flatten (Xlist.map interp (fun interp -> ENIAMcategoriesPL.clarify_categories false senses (lemma,pos,interp)))
  | ENIAMtokenizerTypes.Proper(lemma,pos,interp,_) -> List.flatten (Xlist.map interp (fun interp -> ENIAMcategoriesPL.clarify_categories true senses (lemma,pos,interp)))
  | ENIAMtokenizerTypes.Interp lemma -> ENIAMcategoriesPL.clarify_categories false senses (lemma,"interp",[])
  | _ -> [] *)

(* let create_chart tokens lex_sems paths last =
  ENIAM_LCGrenderer.reset_variable_numbers ();
  let chart = ENIAM_LCGchart.make last in
  let chart = Xlist.fold paths chart (fun chart (id,lnode,rnode) ->
      let t = ExtArray.get tokens id in
      let s = ExtArray.get lex_sems id in
      ENIAM_LCGrenderer.reset_variable_names ();
      ENIAM_LCGrenderer.add_variable_numbers ();
      let cats = clarify_categories ["X"] t in
      let l = ENIAM_LCGlexicon.create_entries rules id t.ENIAMtokenizerTypes.orth cats s.ENIAMlexSemanticsTypes.schemata in
      ENIAM_LCGchart.add_inc_list chart lnode rnode l 0) in
  chart *)

(*let rec split_sons left id right = function
    [] -> List.rev (List.sort compare left), List.sort compare right
  | x :: l -> if x < id then split_sons (x :: left) id right l else split_sons left id (x :: right) l

let rec dep_create_rec nodes sons conll_id =
  let node = IntMap.find nodes conll_id in
  let l = try IntMap.find sons conll_id with Not_found -> [] in
  let left,right = split_sons [] conll_id [] l in
  (* Printf.printf "dep_create_rec [%s] %d [%s]\n" (String.concat ";" (Xlist.map left string_of_int)) conll_id (String.concat ";" (Xlist.map right string_of_int)); *)
  DepNode(conll_id, Xlist.map left (dep_create_rec nodes sons), node, Xlist.map right (dep_create_rec nodes sons))

let create_dep_chart tokens lex_sems paths =
  (* print_endline "create_dep_chart 1"; *)
  let sons = Int.fold 1 (Array.length paths - 1) IntMap.empty (fun sons i ->
      let _,super,_ = paths.(i) in
      IntMap.add_inc sons super [i] (fun l -> i :: l)) in
  (* print_endline "create_dep_chart 2"; *)
  let nodes = Int.fold 0 (Array.length paths - 1) IntMap.empty (fun nodes i ->
      let id,_,_ = paths.(i) in
      let t = ExtArray.get tokens id in
      let s = ExtArray.get lex_sems id in
      ENIAM_LCGrenderer.reset_variable_names ();
      ENIAM_LCGrenderer.add_variable_numbers ();
      let cats = clarify_categories ["X"] t in
      let l = ENIAM_LCGlexicon.create_entries dep_rules id t.ENIAMtokenizerTypes.orth cats s.ENIAMlexSemanticsTypes.schemata s.ENIAMlexSemanticsTypes.lex_entries in
      IntMap.add nodes i l) in
  (* print_endline "create_dep_chart 3"; *)
  let x = dep_create_rec nodes sons 0 in
  (* print_endline "create_dep_chart 4"; *)
  x*)


(* let test_example path id tokens lex_sems paths last =
  ENIAM_LCGreductions.reset_variant_label ();
  let chart = create_chart tokens lex_sems paths last in
  ENIAM_LCGlatexOf.print_chart path (id^"1_chart") "a1" chart;
  let chart,references = ENIAM_LCGchart.lazify chart in
  ENIAM_LCGlatexOf.print_chart path (id^"2_chart") "a4" chart;
  ENIAM_LCGlatexOf.print_references path (id^"2_references") "a4" references;
  let chart = ENIAM_LCGchart.parse chart references 30. Sys.time in (* uwaga: niejawna zmiana imperatywna w references *)
  ENIAM_LCGlatexOf.print_chart path (id^"3_chart") "a4" chart;
  ENIAM_LCGlatexOf.print_references path (id^"3_references") "a4" references;
  if ENIAM_LCGchart.is_parsed chart then (
    let term = ENIAM_LCGchart.get_parsed_term chart in
    Xlatex.latex_file_out path (id^"4_term") "a4" false (fun file ->
        Printf.fprintf file "\\[%s\\]\n" (ENIAM_LCGlatexOf.linear_term 0 term));
    Xlatex.latex_compile_and_clean path (id^"4_term");
    let dependency_tree = ENIAM_LCGreductions.reduce term references in
    ENIAM_LCGlatexOf.print_dependency_tree path (id^"4_dependency_tree") "a0" dependency_tree;
    if ENIAM_LCGreductions.is_reduced_dependency_tree dependency_tree then (
      ENIAM_LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w dependency_tree *)
      ENIAM_LCGlatexOf.print_dependency_tree path (id^"5_dependency_tree") "a4" dependency_tree;
      ENIAM_LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w dependency_tree *)
      ENIAM_LCGlatexOf.print_dependency_tree path (id^"6_dependency_tree") "a4" dependency_tree;
      ENIAM_LCGgraphOf.print_dependency_tree path (id^"6_dependency_tree") dependency_tree;
      ENIAM_LCGgraphOf.print_simplified_dependency_tree path (id^"6_simple_dependency_tree") dependency_tree;
      ())
    else print_endline "not reduced")
  else print_endline "not parsed" *)

(* let rec test_dep_example path id tokens lex_sems first_try paths =
  (* print_endline "test_dep_example 1"; *)
  let paths = CONLL_adapter.convert_dep_tree path first_try paths tokens in
  try
    ENIAM_LCGreductions.reset_variant_label ();
    (* print_endline "test_dep_example 2"; *)
    (* ENIAMsubsyntaxHTMLof.print_dep_sentence path (id^"1_paths") tokens paths; *)
    let chart = create_dep_chart tokens lex_sems paths in
    (* ENIAM_LCGlatexOf.print_dep_chart path (id^"1_chart") "a1" chart; *)
    let chart,references = ENIAM_LCGchart.dep_lazify chart in
    (* ENIAM_LCGlatexOf.print_dep_chart path (id^"2_chart") "a4" chart; *)
    (* ENIAM_LCGlatexOf.print_references path (id^"2_references") "a4" references; *)
    let chart = ENIAM_LCGchart.dep_parse chart references 30. Sys.time in (* uwaga: niejawna zmiana imperatywna w references *)
    (* ENIAM_LCGlatexOf.print_chart path (id^"3_chart") "a4" chart; *)
    (* ENIAM_LCGlatexOf.print_references path (id^"3_references") "a4" references; *)
    if ENIAM_LCGchart.is_dep_parsed chart then (
      let term = ENIAM_LCGchart.get_dep_parsed_term chart in
      (* Xlatex.latex_file_out path (id^"4_term") "a4" false (fun file ->
          Printf.fprintf file "\\[%s\\]\n" (ENIAM_LCGlatexOf.linear_term 0 term));
      Xlatex.latex_compile_and_clean path (id^"4_term"); *)
      let dependency_tree = ENIAM_LCGreductions.reduce term references in
      (* ENIAM_LCGlatexOf.print_dependency_tree path (id^"4_dependency_tree") "a0" dependency_tree; *)
      if ENIAM_LCGreductions.is_reduced_dependency_tree dependency_tree then (
        ENIAM_LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w dependency_tree *)
        (* ENIAM_LCGlatexOf.print_dependency_tree path (id^"5_dependency_tree") "a4" dependency_tree; *)
        ENIAM_LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w dependency_tree *)
        (* ENIAM_LCGlatexOf.print_dependency_tree path (id^"6_dependency_tree") "a4" dependency_tree; *)
        (* ENIAM_LCGgraphOf.print_dependency_tree path (id^"6_dependency_tree") dependency_tree; *)
        (* ENIAM_LCGgraphOf.print_simplified_dependency_tree path (id^"6_simple_dependency_tree") dependency_tree; *)
        ())
      else print_endline "not reduced")
    else (print_endline "not parsed";
      parsed := 0)
  with NotDepParsed(id_ndp,left,l,right) -> (
    if (first_try)
    then test_dep_example path id tokens lex_sems false paths
    else (print_endline "not parsed 2";
      parsed := 0;
      ENIAM_LCGlatexOf.print_not_parsed_dep_chart path (id^"3_not_parsed_chart") "a2" (id_ndp,left,l,right)))

let rec parse_sentence name id tokens lex_sems = function
    RawSentence s -> id
  | StructSentence(paths,last) ->
    (* test_example ("results/" ^ name^"/") (string_of_int id ^ "_") tokens lex_sems paths last; *)
    id + 1
  | DepSentence(paths) ->
    (* test_dep_example ("results/" ^ name ^ "/") (string_of_int id ^ "_") tokens lex_sems true paths; *)
    ENIAMexec.conll_parse_sentence 30. 1 (*("results/" ^ name ^ "/") (string_of_int id ^ "_")*) dep_rules tokens lex_sems (*false*) paths;
    id + 1
  | QuotedSentences sentences ->
    Xlist.fold sentences id (fun id p ->
        parse_sentence name id tokens lex_sems p.sentence)
  | AltSentence l ->
    Xlist.fold l id (fun id (mode,sentence) ->
        parse_sentence name id tokens lex_sems sentence)

let rec parse_paragraph name id tokens lex_sems = function
    RawParagraph s -> id
  | StructParagraph sentences ->
    Xlist.fold sentences id (fun id p ->
        parse_sentence name id tokens lex_sems p.sentence)
  | AltParagraph l ->
    Xlist.fold l id (fun id (mode,paragraph) ->
        parse_paragraph name id tokens lex_sems paragraph)

let rec parse_text name id tokens lex_sems = function
    RawText s -> id
  | StructText paragraphs ->
    Xlist.fold paragraphs id (fun id paragraph ->
        parse_paragraph name id tokens lex_sems paragraph)
  | AltText l ->
    Xlist.fold l id (fun id (mode,text) ->
        parse_text name id tokens lex_sems text) *)

let id_counter = ref 0

let get_id () =
  incr id_counter;
  "ID_" ^ (string_of_int !id_counter)

let get_query_id = function
    AltText[_;CONLL,StructText[StructParagraph[p]]],_ -> if p.id = "" then get_id () else p.id
  | AltText[CONLL,StructText[StructParagraph[p]]],_ -> if p.id = "" then get_id () else p.id
  | _ -> failwith "get_query_id"

let process_id s =
  if Xstring.check_prefix "ID_" s then s else
    let a,b,c = match Xstring.split_delim "/" s with
        [a;b;c] -> a,b,c
      | _ -> failwith ("process_id: " ^ s) in
    if Xstring.check_prefix "NKJP_1M_" a && Xstring.check_prefix "morph_" b && Xstring.check_sufix "-p" b &&
       Xstring.check_prefix "morph_" c && Xstring.check_sufix "-s" c then
      Xstring.cut_prefix "NKJP_1M_" a ^ "." ^ Xstring.cut_sufix "-s" (Xstring.cut_prefix "morph_" c)
    else failwith ("process_id: " ^ s)

let process_conll_corpus filename =
  (* let escaped str = String.map (fun ch -> if ch = ' ' then '/' else
                                  if ch = '(' || ch = ')' then '_' else ch) (String.escaped str) in
  let sort_results folder path id str_query =
    (ignore (Sys.command ("mkdir -p " ^ "results/" ^ (escaped folder) ^ "/" ^ id ^ "/"));
    ignore (Sys.command ("cp -r " ^ path ^ " results/" ^ (escaped folder) ^ "/" ^ id ^ "/"));
    ignore (Sys.command ("rm -r " ^ path));
    let oc = open_out_gen [Open_append; Open_text; Open_creat] 0o640 ("results/" ^ folder ^ "/sentences.txt") in
    output_string oc str_query;
    close_out oc) in *)

  print_endline "process_conll_corpus 1";
  let corpus = File.file_in filename (fun file -> CONLL.match_corpus (CONLL.load_corpus file)) in
  print_endline "process_conll_corpus 2";
  (* let corpus = [List.hd corpus] in *)
  Xlist.iter corpus (fun query ->
    (* parsed := 1; *)
    let id = process_id (get_query_id query) in
    let path = "results/" ^ id ^ "/" in
    ignore (Sys.command ("mkdir -p " ^ path));
    match query with
    | AltText[Raw,RawText query;CONLL,StructText[
        StructParagraph[{sentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence [dep_paths]]} as p]]],tokens ->
        let _ = print_endline @@ CONLL.string_of_sentence_env CONLL tokens p in
        (* (try *)
          let paths1 = CONLL_adapter.convert_dep_tree "" true dep_paths tokens in
          let paths2 = CONLL_adapter.convert_dep_tree "" false dep_paths tokens in
          print_endline ("\nPróba sparsowania zdania:\n" ^ text ^ "\n");
          (* let m_dep_paths = Array.map (fun (id,_,_) -> id,-1,"") dep_paths in *)
          let conll = StructParagraph[{p with sentence = AltSentence([Raw, RawSentence text; CONLL, DepSentence [paths1;paths2]]
          (*@ if Paths.config.Paths.mate_parser_enabled then [Mate, DepSentence m_dep_paths] else []*))}] in
          let text,tokens = ENIAMsubsyntax.parse_text_tokens false false tokens query in
          let sentences = match text with
              AltText[Raw,RawText _; Struct,StructText[AltParagraph[Raw,RawParagraph _; Struct,StructParagraph sentences]]] -> sentences
            | _ -> failwith "process_conll_corpus 1" in
          let text = AltText[Raw,RawText query; Struct, StructText([
              AltParagraph[Raw,RawParagraph query; ENIAM, StructParagraph sentences; CONLL, conll]])] in
    (* print_endline (ENIAMsubsyntaxStringOf.token_extarray tokens);
    print_endline "";
    print_endline (ENIAMsubsyntaxStringOf.text "" tokens text);
    print_endline "";           *)
          let lex_sems = ENIAMlexSemantics.assign tokens text in
          (* ignore(parse_text id 1 tokens lex_sems text); *)
          let text = ENIAMexec.translate_text text in
          let text = ENIAMexec.parse 30. 1 2 rules dep_rules tokens lex_sems text in
          let statuses = ENIAMexecTypes.fold_text ENIAMexecTypes.Struct [] (fun mode acc -> function
              ENIAMexecTypes.ENIAMSentence parse_result -> parse_result.ENIAMexecTypes.status :: acc
            | _ -> acc) text in
          List.iter (fun x -> print_string (ENIAMvisualization.string_of_status x ^ " ")) statuses;
          print_newline ()
          (* if !parsed = 1
          then sort_results "Parsed" path id str_query
          else sort_results "Not_parsed" path id str_query *)
        (* with
          Failure e -> (sort_results ("Failure_" ^ (escaped e)) path id str_query;
            print_endline ("Failure " ^ e))
        | e -> (sort_results (escaped @@ Printexc.to_string e) path id str_query;
          print_endline (Printexc.to_string e))) *)
    | _ -> failwith "process_conll_corpus 2")

let process_conll_corpus2 filename =
  print_endline "process_conll_corpus2 1";
  let corpus = File.file_in filename (fun file -> CONLL.match_corpus (ENIAM_CONLL.load_corpus file)) in
  print_endline "process_conll_corpus2 2";
  (* let corpus = [List.hd corpus] in *)
  let corpus = List.rev (Xlist.rev_map corpus (fun query ->
    (* parsed := 1; *)
    let id = process_id (get_query_id query) in
    let path = "results/" ^ id ^ "/" in
    ignore (Sys.command ("mkdir -p " ^ path));
    match query with
    | AltText[Raw,RawText query;CONLL,StructText[
        StructParagraph[{sentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence [dep_paths]]} as p]]],tokens ->

       (* let text,tokens,msg =
         if !sentence_split = Full then ENIAMsubsyntax.catch_parse_text true !par_names text
         else ENIAMsubsyntax.catch_parse_text false !par_names text in
       let text,msg =
         if msg <> "" || not !perform_integration then text,msg else
         ENIAMpreIntegration.catch_parse_text ENIAMsubsyntaxTypes.Struct tokens text in
       let lex_sems,msg =
         if msg <> "" then ExtArray.make 0 ENIAMlexSemanticsTypes.empty_lex_sem, msg
         else ENIAMlexSemantics.catch_assign tokens text in *)

          let _ = print_endline @@ CONLL.string_of_sentence_env CONLL tokens p in
          let paths1 = CONLL_adapter.convert_dep_tree "" true dep_paths tokens in
          let paths2 = CONLL_adapter.convert_dep_tree "" false dep_paths tokens in
          print_endline ("\nPróba sparsowania zdania:\n" ^ text ^ "\n");
          let conll = StructParagraph[{p with sentence = AltSentence([Raw, RawSentence text; CONLL, DepSentence [paths1;paths2]])}] in
          (* let text,tokens = ENIAMsubsyntax.parse_text_tokens false false tokens query in
          let sentences = match text with
              AltText[Raw,RawText _; Struct,StructText[AltParagraph[Raw,RawParagraph _; Struct,StructParagraph sentences]]] -> sentences
            | _ -> failwith "process_conll_corpus 1" in *)
          let text = AltText[Raw,RawText query; Struct, StructText([
              AltParagraph[Raw,RawParagraph query; (*ENIAM, StructParagraph sentences;*) CONLL, conll]])] in
          print_endline (ENIAMsubsyntaxStringOf.text "" tokens text);
          print_endline (ENIAMsubsyntaxStringOf.token_extarray tokens);
          (* let lex_sems = ENIAMlexSemantics.assign tokens text in *)
          let lex_sems = ENIAMdomainLexSemantics.assign2 tokens text in
          let text = ENIAMexec.translate_text text in
          let text = ENIAMexec.parse 30. 1 2 rules dep_rules tokens lex_sems text in
          (match text with
            ENIAMexecTypes.AltText[ENIAMexecTypes.Raw,ENIAMexecTypes.RawText _; ENIAMexecTypes.Struct,ENIAMexecTypes.StructText[p]] -> p
          | _ -> failwith "process_conll_corpus2 3")
    | _ -> failwith "process_conll_corpus2 2")) in
  let corpus = ENIAMexecTypes.AltText[ENIAMexecTypes.Raw,ENIAMexecTypes.RawText ""; ENIAMexecTypes.Struct,ENIAMexecTypes.StructText corpus] in
  ENIAMvisualization.print_html_text "results/" "parsed_corpus" corpus 1 1 (ExtArray.make 1 ENIAMtokenizerTypes.empty_token_env)


let _ =
  print_endline "test_conll 1";
  Printexc.record_backtrace true;
  print_endline "test_conll 2";
  (* ENIAMlexSemantics.initialize (); *)
  ENIAMsemTypes.user_ontology_flag := true;
  ENIAMcategoriesPL.initialize ();
  ENIAMsemLexicon.sem_lexicon := ENIAMsemLexicon.load_lexicon "data/sem-lexicon.dic";
  ENIAMdomainLexSemantics.initialize2 ();
  print_endline "test_conll 3";
  (* LCGfields.reset (); *)
  (* process_conll_corpus "../../NLP resources/skladnica_zaleznosciowa.conll"; *)
  (* process_conll_corpus "resultsFF1/Failure_find_father1/sentences.txt"; *)
   (* process_conll_corpus2 "../testy/skladnica-test1.conll"; *)
   process_conll_corpus2 "../testy/skladnica-test1b.conll";
  (* process_conll_corpus "../testy/skladnica-test1-Not_found.conll"; *)
  (* process_conll_corpus "../testy/zdania_generujace_blady1/sentences5.txt";*)
  (* LCGfields.print_results () *)*)

(*let rec allign_tokens2 = function
    tokens,[] ->
  | s :: tokens,Token t :: text ->
      if s.orth = t.orth && s.beg = t.beg && s.len = t.len then (s,t) :: allign_tokens2 (tokens,text) else raise
  | s :: tokens,Seq l :: text ->
  | s :: tokens,Variant l :: text ->

let rec allign_tokens = function
  | s :: tokens, t :: text ->
     let beg =  in
     if s.beg = beg then allign_tokens2
  if

let validateTokenizer corpus =
  Xlist.iter corpus (fun (conll_sentence, text, orig, tokens) ->
    let tokens = Array.to_list (ExtArray.to_list tokens) in
    let text = ENIAMtokenizer.tokenize text in
    allign_tokens (tokens,text))*)

let rec split_list n rev l =
  if n = 0 then List.rev rev, l else
  if l = [] then List.rev rev, l else
  split_list (n-1) (List.hd l :: rev) (List.tl l)

let get_interval beg len l =
  let _,l = split_list beg [] l in
  fst (split_list len [] l)

let _ =
  (* ENIAMtokenizer.initialize (); *)
  (* let corpus1 = File.file_in "../../NLP resources/PolEval/PDBUD_A_dev.conllu" (fun file -> (*CONLL2.match_corpus*) (CONLL2.load_corpus file)) in
  let corpus2 = File.file_in "../../NLP resources/PolEval/PDBUD_A_train.conllu" (fun file -> (*CONLL2.match_corpus*) (CONLL2.load_corpus file)) in *)
  let corpus1 = File.file_in "../../NLP resources/PolEval/PDBUD_B_dev.conllu" (fun file -> (*CONLL2.match_corpus*) (CONLL2.load_corpus file)) in
  let corpus2 = File.file_in "../../NLP resources/PolEval/PDBUD_B_train.conllu" (fun file -> (*CONLL2.match_corpus*) (CONLL2.load_corpus file)) in
  let corpus = corpus1 @ corpus2 in
  (* let corpus = List.rev (get_interval 200 20 corpus) in *)
  (* let corpus = File.file_in "../../NLP resources/PolEval/test.conllu" (fun file -> (*CONLL2.match_corpus*) (CONLL2.load_corpus file)) in *)
  (* let corpus = File.file_in "../../NLP resources/PolEval/test2.conllu" (fun file -> (*CONLL2.match_corpus*) (CONLL2.load_corpus file)) in *)
  (* let corpus = [List.hd corpus1] in *)
  (* CONLL2.verify_lengths corpus; *)
  (* let stats = CONLL2.get_tagset corpus in *)
  let corpus = CONLL2.convert_tagset corpus in
  (* let stats = CONLL2.list_dependencies corpus in *)
  let corpus = CONLL2.make_trees corpus in
  (* let stats = CONLL2.list_dependencies_tree corpus in *)
  (* CONLL2.parse corpus; *)
  let stats = CONLL2.extract_rules corpus in
  (* let stats2 = CONLL2.list_dependencies_tree2 corpus in *)
  (* let stats = ValidateTokenizer.validate_segmentation StringQMap.empty corpus in *)
  (* let stats = ValidateTokenizer.count_variants StringQMap.empty corpus in *)
  (* ValidateTokenizer.test_annotate corpus; *)
  let stats = StringQMap.fold stats [] (fun stats k v -> (v,k) :: stats) in
  Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%d\t%s\n" v k);
  (* Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%s\n" k); *)
  (* let stats2 = StringMap.fold stats2 [] (fun stats2 k l -> (Xlist.size l,k,Xlist.sort l compare) :: stats2) in
  Xlist.iter (Xlist.sort stats2 compare) (fun (v,k,l) -> Printf.printf "\n\n%d\t%s\n\t%s\n" v k (String.concat "\n\t" l)); *)
  flush stdout;
  (* ignore(Sys.command "mpg123 \"/home/yacheu/Dokumenty/Inne/gong/gong_00m_30s.mp3\""); *)
  ()
