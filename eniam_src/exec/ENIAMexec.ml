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

open ENIAM_LCGtypes
open ENIAMexecTypes
open Xstd

let string_of_exn = function
    Failure s -> Scanf.unescaped s
  | e -> Printexc.to_string e

let translate_mode = function
    ENIAMsubsyntaxTypes.Raw -> Raw
  | ENIAMsubsyntaxTypes.Struct -> Struct
  | ENIAMsubsyntaxTypes.CONLL -> CONLL
  | ENIAMsubsyntaxTypes.ENIAM -> ENIAM
  | ENIAMsubsyntaxTypes.Mate -> Mate
  | ENIAMsubsyntaxTypes.Swigra -> Swigra
  | ENIAMsubsyntaxTypes.POLFIE -> POLFIE
  | ENIAMsubsyntaxTypes.Error -> Error
  | ENIAMsubsyntaxTypes.Name -> Name
  | ENIAMsubsyntaxTypes.Identifier -> Identifier

let rec translate_sentence = function
    ENIAMsubsyntaxTypes.RawSentence s -> RawSentence s
  | ENIAMsubsyntaxTypes.StructSentence(paths,last) -> StructSentence(paths,last)
  | ENIAMsubsyntaxTypes.DepSentence(paths) -> DepSentence(paths)
  | ENIAMsubsyntaxTypes.QuotedSentences sentences ->
      QuotedSentences(Xlist.map sentences (fun p ->
        {id=p.ENIAMsubsyntaxTypes.id; beg=p.ENIAMsubsyntaxTypes.beg; len=p.ENIAMsubsyntaxTypes.len; next=p.ENIAMsubsyntaxTypes.next; file_prefix=p.ENIAMsubsyntaxTypes.file_prefix;
         sentence=translate_sentence p.ENIAMsubsyntaxTypes.sentence}))
  | ENIAMsubsyntaxTypes.AltSentence l -> AltSentence(Xlist.map l (fun (mode,sentence) ->
      translate_mode mode, translate_sentence sentence))
  | ENIAMsubsyntaxTypes.ErrorSentence s -> ErrorSentence s

let rec translate_paragraph = function
    ENIAMsubsyntaxTypes.RawParagraph s -> RawParagraph s
  | ENIAMsubsyntaxTypes.StructParagraph sentences ->
      StructParagraph(Xlist.map sentences (fun p ->
        {id=p.ENIAMsubsyntaxTypes.id; beg=p.ENIAMsubsyntaxTypes.beg; len=p.ENIAMsubsyntaxTypes.len; next=p.ENIAMsubsyntaxTypes.next; file_prefix=p.ENIAMsubsyntaxTypes.file_prefix;
         sentence=translate_sentence p.ENIAMsubsyntaxTypes.sentence}))
  | ENIAMsubsyntaxTypes.AltParagraph l -> AltParagraph(Xlist.map l (fun (mode,paragraph) ->
      translate_mode mode, translate_paragraph paragraph))
  | ENIAMsubsyntaxTypes.ErrorParagraph s -> ErrorParagraph s

let rec translate_text = function
    ENIAMsubsyntaxTypes.RawText s -> RawText s
  | ENIAMsubsyntaxTypes.StructText paragraphs ->
      StructText(Xlist.map paragraphs translate_paragraph)
  | ENIAMsubsyntaxTypes.AltText l -> AltText(Xlist.map l (fun (mode,text) ->
      translate_mode mode, translate_text text))
  | ENIAMsubsyntaxTypes.ErrorText s -> ErrorText s

let clarify_categories cats (*snode*) token =
  match token.ENIAMtokenizerTypes.token with
    ENIAMtokenizerTypes.Lemma(lemma,pos,interp) ->
      List.flatten (Xlist.map interp (fun interp -> List.flatten (Xlist.map cats (fun (cat,coerced) ->
        (* Printf.printf "lemma=%s pos=%s cat=%s coerced=%s\n%!" lemma pos cat (String.concat "," coerced); *)
        ENIAMcategoriesPL.clarify_categories false cat coerced (*snode*) (lemma,pos,interp)))))
(*  | ENIAMtokenizerTypes.Proper(lemma,pos,interp,senses2) ->
      List.flatten (Xlist.map interp (fun interp -> List.flatten (Xlist.map cats (fun (cat,coerced) -> ENIAMcategoriesPL.clarify_categories true cat coerced (*snode*) (lemma,pos,interp)))))*)
  | ENIAMtokenizerTypes.Interp lemma ->
      List.flatten (Xlist.map cats (fun (cat,coerced) -> ENIAMcategoriesPL.clarify_categories false cat coerced (*snode*) (lemma,"interp",[])))
  | _ -> []

let create_chart rules tokens lex_sems paths last max_cost =
  ENIAM_LCGrenderer.reset_variable_numbers ();
  let chart = ENIAM_LCGchart.make last max_cost in
  let chart = Xlist.fold paths chart (fun chart (id,lnode,rnode) ->
      let t = ExtArray.get tokens id in
      let s = ExtArray.get lex_sems id in
(*       print_endline ("create_chart: orth=" ^ t.ENIAMtokenizerTypes.orth ^ " lemma=" ^ ENIAMtokens.get_lemma t.ENIAMtokenizerTypes.token ^ " |schemata|=" ^ string_of_int (Xlist.size s.ENIAMlexSemanticsTypes.schemata)); *)
      ENIAM_LCGrenderer.reset_variable_names ();
      ENIAM_LCGrenderer.add_variable_numbers ();
      (* if s.ENIAMlexSemanticsTypes.schemata = [] then failwith ("create_chart: no schema for token=" ^ t.ENIAMtokenizerTypes.orth ^ " lemma=" ^ ENIAMtokens.get_lemma t.ENIAMtokenizerTypes.token) else *)
      Xlist.fold s.ENIAMlexSemanticsTypes.schemata chart (fun chart (selectors,cats,(*snode,*)local_schema,schema,distant_schema) ->
        let cats = clarify_categories cats (*snode*) t in
      (* let chart = ENIAM_LCGchart.add_inc_list chart lnode rnode s.ENIAMlexSemanticsTypes.lex_entries 0 in *)
        let l = ENIAM_LCGlexicon.create_entries rules id t.ENIAMtokenizerTypes.orth cats [selectors,local_schema,schema,distant_schema] s.ENIAMlexSemanticsTypes.lex_entries in
        Xlist.fold l chart (fun chart (v,cost) ->
          ENIAM_LCGchart.add_inc chart lnode rnode cost v 0))) in
  chart

let rec split_sons left id right = function
    [] -> List.rev (List.sort compare left), List.sort compare right
  | x :: l -> if x < id then split_sons (x :: left) id right l else split_sons left id (x :: right) l

let rec dep_create_rec nodes sons conll_id =
  let node = try IntMap.find nodes conll_id with Not_found -> failwith ("dep_create_rec: unknown node " ^ string_of_int conll_id) in
  let l = try IntMap.find sons conll_id with Not_found -> [] in
  let left,right = split_sons [] conll_id [] l in
  (* Printf.printf "dep_create_rec [%s] %d [%s]\n" (String.concat ";" (Xlist.map left string_of_int)) conll_id (String.concat ";" (Xlist.map right string_of_int)); *)
  DepNode(conll_id, Xlist.map left (dep_create_rec nodes sons), node, Xlist.map right (dep_create_rec nodes sons))

let create_dep_chart dep_rules tokens lex_sems paths =
  print_endline "create_dep_chart 1";
  let sons = Int.fold 1 (Array.length paths - 1) IntMap.empty (fun sons i ->
      let _,sl,_ = paths.(i) in
      let super,_ = List.hd sl in (* FIXME: to trzeba poprawić przy współdzielonych podrzędnikach *)
      IntMap.add_inc sons super [i] (fun l -> i :: l)) in
  print_endline "create_dep_chart 2";
  let nodes = Int.fold 0 (Array.length paths - 1) IntMap.empty (fun nodes i ->
      let id,_,_ = paths.(i) in
      let t = ExtArray.get tokens id in
      let s = ExtArray.get lex_sems id in
      ENIAM_LCGrenderer.reset_variable_names ();
      ENIAM_LCGrenderer.add_variable_numbers ();
      if s.ENIAMlexSemanticsTypes.schemata = [] then failwith ("create_dep_chart: no schema for token=" ^ t.ENIAMtokenizerTypes.orth ^ " lemma=" ^ ENIAMtokens.get_lemma t.ENIAMtokenizerTypes.token) else
      Xlist.fold s.ENIAMlexSemanticsTypes.schemata nodes (fun nodes (selectors,cats,(*snode,*)local_schema,schema,distant_schema) ->
        let cats = clarify_categories ["X",["X"]] (*snode*) t in
      (* let chart = ENIAM_LCGchart.add_inc_list chart lnode rnode s.ENIAMlexSemanticsTypes.lex_entries 0 in *)
        let l = ENIAM_LCGlexicon.create_entries dep_rules id t.ENIAMtokenizerTypes.orth cats [selectors,local_schema,schema,distant_schema] s.ENIAMlexSemanticsTypes.lex_entries in
        let l = Xlist.rev_map l fst in
        IntMap.add_inc nodes i l (fun l2 -> l @ l2))) in
  print_endline "create_dep_chart 3";
  let x = dep_create_rec nodes sons 0 in
  print_endline "create_dep_chart 4";
  x

let create_text_fragments tokens paths last =
  try 
  let text_fragments = Array.make last IntMap.empty in
  Xlist.iter paths (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    let orth = if t.ENIAMtokenizerTypes.beg + t.ENIAMtokenizerTypes.len = t.ENIAMtokenizerTypes.next
      then t.ENIAMtokenizerTypes.orth else t.ENIAMtokenizerTypes.orth ^ " " in
    text_fragments.(lnode) <- IntMap.add text_fragments.(lnode) rnode orth);
  Int.iter_down 0 (last - 1) (fun i ->
    let map = IntMap.fold text_fragments.(i) text_fragments.(i) (fun map j orth ->
      if j = last then map else
      IntMap.fold text_fragments.(j) map (fun map k orth2 ->
        IntMap.add map k (orth ^ orth2))) in
    text_fragments.(i) <- map);
  text_fragments
  with e -> print_endline (Printexc.to_string e); failwith "create_text_fragments"

(*let create_beg_positions tokens paths last =
  let beg_positions = Array.make last (-1) in
  Xlist.iter paths (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    beg_positions.(lnode) <- t.ENIAMtokenizerTypes.beg);
  beg_positions

let create_end_positions tokens paths last =
  let end_positions = Array.make last (-1) in
  Xlist.iter paths (fun (id,lnode,rnode) ->
    let t = ExtArray.get tokens id in
    end_positions.(rnode) <- t.ENIAMtokenizerTypes.beg + t.ENIAMtokenizerTypes.len);
  end_positions*)

let eniam_parse_sentence timeout verbosity rules tokens lex_sems paths last max_cost =
  ENIAM_LCGreductions.reset_variant_label ();
  let result = {empty_eniam_parse_result with paths_size = Xlist.size paths} in
  let result = if verbosity = 0 then result else {result with
    text_fragments=create_text_fragments tokens paths last;
    (*beg_positions=create_beg_positions tokens paths last;
    end_positions=create_end_positions tokens paths last;*)} in
(*   if not (ENIAMsubsyntax.is_parsed tokens paths last) then {result with status=NotLemmatized} else *)
  let time1 = time_fun () in
  try
    (* print_endline "eniam_parse_sentence 1"; *)
    let chart = create_chart rules tokens lex_sems paths last max_cost in
    let result = if verbosity = 0 then result else {result with chart1=chart} in
    (* print_endline "eniam_parse_sentence 2"; *)
    let chart,references = ENIAM_LCGchart.lazify chart in
    let result = if verbosity = 0 then result else {result with chart2=ENIAM_LCGchart.copy chart; references2=ExtArray.copy references} in
    (* print_endline "eniam_parse_sentence 3"; *)
    let time2 = time_fun () in
    (* Printf.printf "time2-time1=%f\n%!" (time2 -. time1); *)
    let result = {result with lex_time=time2 -. time1} in
    try
(*     print_endline "eniam_parse_sentence 4"; *)
      let chart = ENIAM_LCGchart.parse !lcg_rules chart references timeout time_fun in (* uwaga: niejawna zmiana imperatywna w references *)
      let time3 = time_fun () in
(*       Printf.printf "time3-time2=%f\n%!" (time3 -. time2); *)
      let result = if verbosity = 0 then result else {result with chart3=chart; references3=ExtArray.copy references} in
      let result = {result with parse_time=time3 -. time2; chart_size=ENIAM_LCGchart.get_no_entries chart} in
(*     print_endline "eniam_parse_sentence 4a"; *)
      let chart,partial = if !partial_parsing_flag && not (ENIAM_LCGchart.is_parsed chart) then ENIAM_LCGchart.merge result.text_fragments chart references,true else chart,false in
(*     print_endline "eniam_parse_sentence 4b"; *)
      if ENIAM_LCGchart.is_parsed chart then
        try
    (* print_endline "eniam_parse_sentence 5"; *)
          let term = ENIAM_LCGchart.get_parsed_term chart in
          let result = if verbosity = 0 then result else {result with term4=term} in
          let dependency_tree = ENIAM_LCGreductions.reduce term references in
          let time4 = time_fun () in
          (* Printf.printf "time4-time3=%f\n%!" (time4 -. time3); *)
          let result = if verbosity = 0 then result else {result with dependency_tree4=Array.copy dependency_tree} in
          let result = {result with reduction_time=time4 -. time3; dependency_tree_size=Array.length dependency_tree} in
              (* print_endline "a 0"; *)
          if ENIAM_LCGreductions.is_reduced_dependency_tree dependency_tree then
            try
    (* print_endline "eniam_parse_sentence 6"; *)
              (* print_endline "a 1"; *)
              ENIAM_LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree5=Array.copy dependency_tree} in
              (* print_endline "a 2"; *)
              ENIAM_LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree6a=dependency_tree} in
              (* print_endline "a 3"; *)
              (* let dependency_tree = ENIAM_LCGreductions.normalize_variants dependency_tree in *) (* FIXME: trzeba zreimplementować obsługę wielokrotnych etykiet *)
              (* print_endline "a 4"; *)
              ENIAMlexSemantics.create_tokens_for_artificial_nodes tokens lex_sems dependency_tree;
              let result = (*if verbosity = 0 then result else*) {result with dependency_tree6b=Array.copy dependency_tree} in
              try
                ENIAM_LCGreductions.validate_dependency_tree dependency_tree;
                let time6 = time_fun () in
                (* Printf.printf "time6-time4=%f\n%!" (time6 -. time4); *)
                {result with status=if partial then PartialParsed else Parsed; sem_time=time6 -. time4}
              with e ->
                let time6 = time_fun () in
                {result with status=ReductionError3; msg=string_of_exn e; sem_time=time6 -. time4}
            with e ->
              let time6 = time_fun () in
              {result with status=ReductionError2; msg=string_of_exn e; sem_time=time6 -. time4}
          else
            {result with status=NotReduced}
        with
        | SemTooBig ->
            let time4 = time_fun () in
            {result with status=TooManyNodes; reduction_time=time4 -. time3}
        | e ->
            let time4 = time_fun () in
            {result with status=ReductionError; msg=string_of_exn e; reduction_time=time4 -. time3}
      else {result with status=NotParsed}
    with
      Timeout t ->
        let time3 = time_fun () in
        {result with status=ParseTimeout; msg=Printf.sprintf "%f" t; parse_time=time3 -. time2}
    | e ->
        let time3 = time_fun () in
        {result with status=ParseError; msg=string_of_exn e; parse_time=time3 -. time2}
  with e ->
    let time2 = time_fun () in
    {result with status=LexiconError; msg=string_of_exn e; lex_time=time2 -. time1}

let conll_parse_sentence timeout verbosity dep_rules tokens lex_sems paths =
  ENIAM_LCGreductions.reset_variant_label ();
  let result = {empty_eniam_parse_result with paths_size = Array.length paths} in
  let result = if verbosity = 0 then result else result(*{result with text_fragments=create_dep_text_fragments tokens paths last}*) in (* FIXME *)
  let time1 = time_fun () in
  try
    (* let paths = (*CONLL_adapter.*)convert_dep_tree "" first_try paths tokens in *)
    let chart = create_dep_chart dep_rules tokens lex_sems paths in
    let result = if verbosity = 0 then result else {result with dep_chart1=chart} in
    let chart,references = ENIAM_LCGchart.dep_lazify chart in
    let result = if verbosity = 0 then result else {result with dep_chart2=chart; references2=ExtArray.copy references} in
    let time2 = time_fun () in
    let result = {result with lex_time=time2 -. time1} in
    try
      let chart = ENIAM_LCGchart.dep_parse chart references timeout time_fun in (* uwaga: niejawna zmiana imperatywna w references *)
      let time3 = time_fun () in
      let result = if verbosity = 0 then result else {result with parsed_dep_chart3=chart; references3=references} in
      let result = {result with parse_time=time3 -. time2; (*chart_size=ENIAM_LCGchart.get_no_entries chart*)} in (* FIXME *)
      if ENIAM_LCGchart.is_dep_parsed chart then
        try
          let term = ENIAM_LCGchart.get_dep_parsed_term chart in
          let result = if verbosity = 0 then result else {result with term4=term} in
          let dependency_tree = ENIAM_LCGreductions.reduce term references in
          let time4 = time_fun () in
          let result = if verbosity = 0 then result else {result with dependency_tree4=Array.copy dependency_tree} in
          let result = {result with reduction_time=time4 -. time3; dependency_tree_size=Array.length dependency_tree} in
          if ENIAM_LCGreductions.is_reduced_dependency_tree dependency_tree then
            try
              ENIAM_LCGreductions.assign_labels dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree5=Array.copy dependency_tree} in
              ENIAM_LCGreductions.remove_cuts dependency_tree; (* uwaga: niejawna zmiana imperatywna w result *)
              let result = if verbosity = 0 then result else {result with dependency_tree6a=dependency_tree} in
              let dependency_tree = ENIAM_LCGreductions.normalize_variants dependency_tree in
              let result = (*if verbosity = 0 then result else*) {result with dependency_tree6b=Array.copy dependency_tree} in
              try
                ENIAM_LCGreductions.validate_dependency_tree dependency_tree;
                let time6 = time_fun () in
                {result with status=Parsed; sem_time=time6 -. time4}
              with e ->
                let time6 = time_fun () in
                {result with status=ReductionError3; msg=string_of_exn e; sem_time=time6 -. time4}
            with e ->
              let time6 = time_fun () in
              {result with status=ReductionError2; msg=string_of_exn e; sem_time=time6 -. time4}
          else
            {result with status=NotReduced}
        with
        | SemTooBig ->
            let time4 = time_fun () in
            {result with status=TooManyNodes; reduction_time=time4 -. time3}
        | e ->
            let time4 = time_fun () in
            {result with status=ReductionError; msg=string_of_exn e; reduction_time=time4 -. time3}
      else {result with status=NotParsed}
    with
      Timeout t ->
        let time3 = time_fun () in
        {result with status=ParseTimeout; msg=Printf.sprintf "%f" t; parse_time=time3 -. time2}
    | NotDepParsed(id_ndp,left,l,right) ->
        let time3 = time_fun () in
        {result with status=NotParsed; not_parsed_dep_chart3=(id_ndp,left,l,right); parse_time=time3 -. time2}
    | e ->
        let time3 = time_fun () in
        {result with status=ParseError; msg=string_of_exn e; parse_time=time3 -. time2}
  with e ->
    let time2 = time_fun () in
    {result with status=LexiconError; msg=string_of_exn e; lex_time=time2 -. time1}

(*let swigra_in, swigra_out = (*Unix.open_process "../swigra/parser/run.sh"*)
  if Paths.config.Paths.swigra_enabled then
    Unix.open_process (Paths.config.Paths.swigra_path ^ "run.sh")
  else stdin, stdout
*)

let polfie_results = "results/polfie"

let parse_polfie_sentence s =
  let sentence, choices, fstructure, form = XTNormalizer.load_fstructure (polfie_results ^ "/fstructure-1.pl") in
  XTXmlOf.print (polfie_results ^ "/fstructure-1.xml") sentence choices fstructure form;
 (* ignore @@ Sys.command("rm " ^ polfie_results ^ "/fstructure-1.pl"); *)
 (* let converted = XTToOcaml.lfg_to_conll fstructure in *)
  let linear_term = XTLinearTermOf.from_lfg_term fstructure in
  print_endline (ENIAM_LCGstringOf.linear_term 0 linear_term);
  print_endline "Done POLFIE parsing";
  RawSentence s

let parse timeout verbosity max_cost rules dep_rules (*name id*) tokens lex_sems =
  map_text Struct (fun mode -> function
    RawSentence s ->
      (match mode with
(*        Swigra ->
          if not Paths.config.Paths.swigra_enabled then RawSentence s else (
          Printf.fprintf swigra_out "%s\n%!" s;
          print_endline ("swigra: " ^ input_line swigra_in);
          RawSentence s)*)
      | Name -> print_endline s; RawSentence s
      | Identifier -> RawSentence s
      | POLFIE -> parse_polfie_sentence s
      | _ -> RawSentence s)
  | StructSentence(paths,last) ->
      (match mode with
        ENIAM ->
          let result = eniam_parse_sentence timeout verbosity rules tokens lex_sems paths last max_cost in
          ENIAMSentence result
      | _ -> failwith "parse 3")
  | DepSentence paths_list ->
      (match mode with
        CONLL | Mate | Swigra ->
          let result = Xlist.fold paths_list empty_eniam_parse_result (fun result paths ->
            if result.status = Parsed then result else
            conll_parse_sentence timeout verbosity dep_rules tokens lex_sems paths) in
          ENIAMSentence result
    | _ -> failwith "parse 2")
  | ErrorSentence s -> ErrorSentence s
  | _ -> failwith "parse 1")


(*

open Printf

let semantic_processing timeout test_only_flag file_prefix tokens lex_sems max_n dependency_tree =
  let time5 = time_fun () in
  let result = {empty_semantic_processing_result with file_prefix=file_prefix} in
  try
    let (*dependency_tree2*)(*sem*)disamb = LCGvalence.assign_frames_and_senses tokens lex_sems dependency_tree in
    let disamb(*sem*) = DisambSelPref.fit_sel_prefs DisambSelPref.fit_node1 (*dependency_tree2*)disamb in
    let (*sem*)disamb = DisambLemma.disambiguate_nodes (*dependency_tree*)(*sem*)disamb in
    let (*sem*)disamb = DisambLemma.remove_unused(*disambiguate_nodes*) (*dependency_tree*)(*sem*)disamb in
    let (*sem*)disamb = DisambLemma.remove_unused_choices(*disambiguate_nodes*) (*dependency_tree*)(*sem*)disamb in
    let (*disamb*)sem = DisambSelPref.fit_sel_prefs DisambSelPref.fit_node2 (*dependency_tree2*)disamb in
    let result = if test_only_flag then result else {result with disamb=disamb} in
    let sem = print_string " 7"; DisambLemma.disambiguate_meanings (*dependency_tree*)sem in
    let sem(*disamb*) = print_string " 8"; DisambLemma.remove_unused_choices(*disambiguate_nodes*) (*dependency_tree*)sem(*disamb*) in
    let result = if test_only_flag then result else {result with sem=sem} in
    let sem2 = SemGraph.translate tokens lex_sems (*disamb*)sem in
    let result = if test_only_flag then result else {result with sem2=sem2} in
    let sem3(*disamb*) = print_string " 10"; SemGraph.make_tree(*disambiguate_nodes*) (*dependency_tree*)sem2(*disamb*) in
    let sem3(*disamb*) = print_string " 11"; SemGraph.simplify_tree(*disambiguate_nodes*) (*dependency_tree*)sem3(*disamb*) in
(*     let sem3(*disamb*) = SemGraph.manage_quantification(*disambiguate_nodes*) (*dependency_tree*)sem3(*disamb*) in  *)
    let sem3(*disamb*) = print_string " 12"; SemGraph.simplify_gender(*disambiguate_nodes*) (*dependency_tree*)sem3(*disamb*) in
(*     if Array.length disamb < 10000 then print_xml_dependency_tree "results/trees/" (id ^ "dis") disamb; *)
    let result = if test_only_flag then result else {result with sem3=sem3} in
    let time6 = time_fun () in
    if SemGraph.validate_semantics sem3 then
      let trees = print_string " 13"; SemGraph.draw_trees max_n sem3 in
      let trees2 = print_string " 14"; Xlist.map trees SemMrl.variable_alpha_convertion in
      let mrls = print_string " 15"; Xlist.map trees2 SemMrl.make_mrl in
      let mrls = print_string " 16"; Xlist.map mrls SemMrl.move_requirements in
      let mrss = print_string " 17"; Xlist.map mrls SemMrl.make_mrs_of_mrl in
      let mrss = print_string " 18"; Xlist.map mrss SemMrl.mrs_handle_alpha_convertion in
      let fols = print_string " 19"; Xlist.map mrss (fun mrs ->
        let l = SemMrl.foll_of_mrs_greedy mrs in
        if l = [] then failwith "empty fol" else
        List.hd l) in
      let result = print_string " 20"; if test_only_flag then result else {result with trees=trees; mrls=fols(*mrls*)} in
      {result with status=Parsed; sem_time=time6 -. time5}
    else {result with status=NotTranslated; sem_time=time6 -. time5}
  with e ->
    let time6 = time_fun () in
    {result with status=SemError; msg=string_of_exn e; sem_time=time6 -. time5}


let rec semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n = function
    RawSentence s -> RawSentence s
  | ENIAMSentence result -> SemSentence (semantic_processing timeout test_only_flag result.file_prefix tokens lex_sems max_n result.dependency_tree)
  | CONLLSentence result -> SemSentence (semantic_processing timeout test_only_flag result.file_prefix tokens lex_sems max_n result.dependency_tree)
  | QuotedSentences sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n p.psentence in
        {p with psentence=sentence}) in
      QuotedSentences(List.rev sentences)
  | AltSentence l ->
      let l = Xlist.rev_map l (fun (mode,sentence) ->
        mode, semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n sentence) in
      AltSentence(List.rev l)
 | _ -> failwith "semantic_processing_sentence"

let rec semantic_processing_paragraph timeout test_only_flag tokens lex_sems max_n = function
    RawParagraph s -> RawParagraph s
  | StructParagraph sentences ->
      let sentences = Xlist.rev_map sentences (fun p ->
        let sentence = semantic_processing_sentence timeout test_only_flag tokens lex_sems max_n p.psentence in
        {p with psentence=sentence}) in
      StructParagraph(List.rev sentences)
  | AltParagraph l ->
      let l = Xlist.rev_map l (fun (mode,paragraph) ->
        mode, semantic_processing_paragraph timeout test_only_flag tokens lex_sems max_n paragraph) in
      AltParagraph(List.rev l)

let rec semantic_processing_text timeout test_only_flag tokens lex_sems max_n = function
    RawText s -> RawText s
  | StructText paragraphs  ->
      let paragraphs = Xlist.rev_map paragraphs (fun paragraph ->
        semantic_processing_paragraph timeout test_only_flag tokens lex_sems max_n paragraph) in
      StructText(List.rev paragraphs)
  | AltText l -> AltText(Xlist.map l (fun (mode,text) ->
       mode, semantic_processing_text timeout test_only_flag tokens lex_sems max_n text))*)

let eniam_semantic_processing verbosity tokens lex_sems (result : eniam_parse_result) =
  let tree,result =
    try
      let tree = ENIAMsemValence.assign_frames tokens lex_sems result.dependency_tree6b in
      let result = if verbosity < 2 then result else {result with dependency_tree7=tree} in
      tree,result
    with e -> [| |],{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let tree,result =
    try
      let tree = ENIAMsemValence.reduce_tree tokens lex_sems tree in (* FIXME: tokens lex_sems nie są potrzebne *)
      let result = if verbosity < 2 then result else {result with dependency_tree8=tree} in
      tree,result
    with e -> ExtArray.make 0 Dot,{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let result =
    try
      ENIAMsemValence.transfer_attributes tree; (* niejawna zmiana imperatywna w tree *)
      result
    with e -> {result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let tree,result =
    try
      ENIAMdisambiguation.selprefs tree; (* niejawna zmiana imperatywna w tree *)
      let tree = ENIAMdisambiguation.merge tree in
      (* let tree = ENIAMdisambiguation.random_tree tokens lex_sems tree in *) (* FIXME: tokens lex_sems nie są potrzebne *)
      let tree = ENIAM_LCGreductions.reshape_dependency_tree(*ExtArray.to_array*) tree in
      ENIAMlexSemantics.create_tokens_for_artificial_nodes tokens lex_sems tree;
      ENIAMcoreference.resolve tree;
      let result = if verbosity = 0 then result else {result with dependency_tree9=tree} in
      tree,result
    with e -> [| |],{result with status=SemValenceError; msg=string_of_exn e} in
  if result.status = SemValenceError then result else
  let graph,result =
    try
      let graph = ENIAMsemGraph.translate tokens lex_sems tree in (* FIXME: pro nie mają id *)
      let result = if verbosity = 0 then result else {result with semantic_graph10=graph} in
      let graph = ENIAMsemGraph.make_tree graph in
      let result = if verbosity = 0 then result else {result with semantic_graph11=graph} in
      graph,result
    with e -> ENIAMsemTypes.Dot,{result with status=SemGraphError; msg=string_of_exn e} in
  if result.status = SemGraphError then result else
  let r = ref [] in
  (try ENIAMsemGraph.validate_translation r graph with e -> r := string_of_exn e :: !r);
  if !r <> [] then {result with status = SemGraphError; msg=String.concat "<BR>" !r} else
  let graph,result =
    try
      let graph = ENIAMsemGraph.reduce_tree graph in
      let result = (*if verbosity = 0 then result else*) {result with semantic_graph11=graph} in
      graph,result
    with e -> ENIAMsemTypes.Dot,{result with status=SemGraphError; msg=string_of_exn e} in
  if result.status = SemGraphError then result else
  let r = ref [] in
  (try ENIAMsemGraph.validate_reduction r graph with e -> r := string_of_exn e :: !r);
  if !r <> [] then {result with status = SemGraphError; msg=String.concat "<BR>" !r} else
  let graph,result =
    try
      let graph = ENIAMsemGraph.greater_simplify graph in
(*    let graph = ENIAMsemGraph.manage_quantification graph in  *)
      let graph = ENIAMsemGraph.simplify_gender graph in
      let graph = ENIAMsemGraph.manage_variant_labels graph in
      let result = (*if verbosity = 0 then result else*) {result with semantic_graph11=graph; semantic_graph12=graph} in
      graph,result
    with e -> ENIAMsemTypes.Dot,{result with status=SemGraphError; msg=string_of_exn e} in
  if result.status = SemGraphError then result else
  {result with status = if result.status = PartialParsed then PartialSemParsed else SemParsed}

let semantic_processing verbosity tokens lex_sems text =
  map_text Struct (fun mode -> function
      ENIAMSentence result ->
        if result.status <> Parsed || result.status <> PartialParsed then ENIAMSentence result else
        ENIAMSentence (eniam_semantic_processing verbosity tokens lex_sems result)
    | t -> t) text


(*
let rec extract_query_text = function
    RawText s -> s
  | AltText l -> (try extract_query_text (Xlist.assoc l Raw) with Not_found -> failwith "extract_query_text")
  | _ -> failwith "extract_query_text"

let process_query pre_in pre_out timeout test_only_flag id full_query max_n =
  (* print_endline "process_query 0"; *)
  let result = {empty_result with input_text=translate_text (fst full_query)} in
  let time1 = time_fun () in
  (* print_endline "process_query 1"; *)
  Marshal.to_channel pre_out full_query [];
  flush pre_out;
  (* print_endline "process_query 2"; *)
  let pre_text,tokens,lex_sems,msg,pre_time1 = (Marshal.from_channel pre_in :
          ENIAMsubsyntaxTypes.text *
          ENIAMtokenizerTypes.token_record ExtArray.t *
          ENIAMlexSemanticsTypes.lex_sem ExtArray.t * string * float) in
  let time2 = time_fun () in
  let result = if test_only_flag then result else {result with pre_text=translate_text pre_text; tokens=tokens; lex_sems=lex_sems} in
  let result = {result with pre_time1=pre_time1; pre_time2=time2 -. time1} in
  if msg <> "" then {result with status=PreprocessingError; msg=msg} else (
  (* print_endline "process_query 3"; *)
  let parsed_text = parse_text timeout test_only_flag Struct id tokens lex_sems (translate_text pre_text) in
  (* print_endline "process_query 4"; *)
  let time3 = time_fun () in
  let result = if test_only_flag then result else {result with status=Parsed; parsed_text=parsed_text} in
  let result = {result with parse_time=time3 -. time2} in
  (* print_endline "process_query 5"; *)
  let selected_sent_text =
     if not Paths.config.Paths.sentence_selection_enabled then parsed_text
     else select_sentences_text parsed_text in
  (* print_endline "process_query 6"; *)
  let result = if test_only_flag then result else {result with status=Parsed; selected_sent_text=selected_sent_text} in
  let semantic_text = semantic_processing_text timeout test_only_flag tokens lex_sems max_n selected_sent_text in
  (* print_endline "process_query 7"; *)
  let selected_semantic_text =
     if not Paths.config.Paths.sentence_selection_enabled then semantic_text
     else select_sentences_text semantic_text in
  (* print_endline "process_query 8"; *)
  let time4 = time_fun () in
  let result =
    if test_only_flag then result
    else {result with status=Parsed;
      semantic_text=semantic_text;
      selected_semantic_text=selected_semantic_text} in
  let result = {result with semantic_time=time4 -. time3} in
  result)

let print_result file result =
  Printf.fprintf file "query: %s\n" (extract_query_text result.input_text);
  (match result.status with
    Idle -> Printf.fprintf file "idle\n"
  | PreprocessingError -> Printf.fprintf file "error_pre: %s\n" result.msg
  (* | LexiconError -> Printf.fprintf file "error_lex: %s\n" result.msg
  | ParseError -> Printf.fprintf file "error_parse: %s\n" result.msg
  | ParseTimeout -> Printf.fprintf file "timeout: %s\n" result.msg
  | NotParsed -> Printf.fprintf file "not_parsed: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size
  | ReductionError -> Printf.fprintf file "error_reduction: %s\n" result.msg
  | TooManyNodes -> Printf.fprintf file "to_many_nodes: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size
  | NotReduced -> Printf.fprintf file "not_reduced: paths_size=%d chart_size=%d\n" result.paths_size result.chart_size
  | SemError -> Printf.fprintf file "error_sem: %s\n" result.msg
  | NotTranslated -> Printf.fprintf file "not_translated: \n" *)
  (* | Parsed -> Printf.fprintf file "parsed: paths_size=%d chart_size=%d dependency_tree_size=%d\n" result.paths_size result.chart_size result.dependency_tree_size *)
  | Parsed -> Printf.fprintf file "parsed\n"
  | _ -> failwith "print_result");
  (* Printf.fprintf file "times: pre_time1=%f pre_time2=%f lex_time=%f parse_time=%f reduction_time=%f sem_time=%f\n%!"
    result.pre_time1 result.pre_time2 result.lex_time result.parse_time result.reduction_time result.sem_time *)
  Printf.fprintf file "times: pre_time1=%f pre_time2=%f parse_time=%f\n%!"
    result.pre_time1 result.pre_time2 result.parse_time

let add_result sum_result result =
  let sum_result = {sum_result with no_queries=sum_result.no_queries+1} in
  let sum_result = match result.status with
    Idle -> failwith "sum_result"
  | PreprocessingError -> {sum_result with no_pre_error=sum_result.no_pre_error+1}
  (* | LexiconError -> {sum_result with no_lex_error=sum_result.no_lex_error+1}
  | ParseError -> {sum_result with no_parse_error=sum_result.no_parse_error+1}
  | ParseTimeout -> {sum_result with no_timeout=sum_result.no_timeout+1}
  | NotParsed -> {sum_result with no_not_parsed=sum_result.no_not_parsed+1}
  | ReductionError -> {sum_result with no_reduction_error=sum_result.no_reduction_error+1}
  | TooManyNodes -> {sum_result with no_too_many_nodes=sum_result.no_too_many_nodes+1}
  | NotReduced -> {sum_result with no_not_reduced=sum_result.no_not_reduced+1}
  | SemError -> {sum_result with no_sem_error=sum_result.no_sem_error+1}
  | NotTranslated -> {sum_result with no_not_translated=sum_result.no_not_translated+1} *)
  | Parsed -> {sum_result with no_parsed=sum_result.no_parsed+1}
  | _ -> failwith "add_result" in
  {sum_result with
     sum_pre_time1=sum_result.sum_pre_time1 +. result.pre_time1;
     sum_pre_time2=sum_result.sum_pre_time2 +. result.pre_time2;
     (* sum_lex_time=sum_result.sum_lex_time +. result.lex_time; *)
     sum_parse_time=sum_result.sum_parse_time +. result.parse_time;
     (* sum_reduction_time=sum_result.sum_reduction_time +. result.reduction_time;
     sum_sem_time=sum_result.sum_sem_time +. result.sem_time*)}

let print_sum_result file r = failwith "print_sum_result: ni"
  (* Printf.fprintf file "avg_times: pre_time1=%f pre_time2=%f lex_time=%f parse_time=%f reduction_time=%f sem_time=%f\n"
    (r.sum_pre_time1 /. float r.no_queries)
    (r.sum_pre_time2 /. float r.no_queries)
    (r.sum_lex_time /. float r.no_queries)
    (r.sum_parse_time /. float r.no_queries)
    (r.sum_reduction_time /. float r.no_queries)
    (r.sum_sem_time /. float r.no_queries);
  Printf.fprintf file "sum_results: pre_error=%d (%f%%) lex_error=%d (%f%%) parse_error=%d (%f%%) timeout=%d (%f%%) not_parsed=%d (%f%%) reduction_error=%d (%f%%) too_many_nodes=%d (%f%%) not_reduced=%d (%f%%) sem_error=%d (%f%%) not_translated=%d (%f%%) parsed=%d (%f%%)\n%!"
    r.no_pre_error (float r.no_pre_error /. float r.no_queries *. 100.)
    r.no_lex_error (float r.no_lex_error /. float r.no_queries *. 100.)
    r.no_parse_error (float r.no_parse_error /. float r.no_queries *. 100.)
    r.no_timeout (float r.no_timeout /. float r.no_queries *. 100.)
    r.no_not_parsed (float r.no_not_parsed /. float r.no_queries *. 100.)
    r.no_reduction_error (float r.no_reduction_error /. float r.no_queries *. 100.)
    r.no_too_many_nodes (float r.no_too_many_nodes /. float r.no_queries *. 100.)
    r.no_not_reduced (float r.no_not_reduced /. float r.no_queries *. 100.)
    r.no_sem_error (float r.no_sem_error /. float r.no_queries *. 100.)
    r.no_not_translated (float r.no_not_translated /. float r.no_queries *. 100.)
    r.no_parsed (float r.no_parsed /. float r.no_queries *. 100.) *)

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let generate_queries filename timeout =
  let queries = File.load_lines filename in
  List.rev (fst (Xlist.fold queries ([],1) (fun (l,id) query ->
    let query = try List.hd (Str.split (Str.regexp "\t") query) with _ -> "" in
    (string_of_int id,(query,timeout)) :: l, id+1)))

let generate_queries_id filename timeout =
  let queries = File.load_lines filename in
  List.rev (Xlist.rev_map queries (fun line ->
    match Str.split (Str.regexp "\t") line with
      [id;query] -> id,(query,timeout)
    | _ -> failwith ("generate_queries_id: " ^ line)))

(*let test_process_file filename output_filename timeout =
  let queries = generate_queries filename timeout in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  File.file_out output_filename (fun file ->
    let _ = Xlist.fold queries empty_sum_result (fun sum_result (id,(query,timeout)) ->
      let result = process_query ic oc timeout true id query 10 in
      print_result file result;
      let sum_result = add_result sum_result result in
      print_sum_result file sum_result;
      sum_result) in
    ());
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  ()

let process_file_id filename output_filename timeout =
  let queries = generate_queries_id filename timeout in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  File.file_out output_filename (fun file ->
    let _ = Xlist.fold queries empty_sum_result (fun sum_result (id,(query,timeout)) ->
      let result = process_query ic oc timeout true id query 10 in
      print_result file result;
      let sum_result = add_result sum_result result in
      print_sum_result file sum_result;
      sum_result) in
    ());
  Printf.fprintf oc "\n%!";
  let _ = Unix.shutdown_connection ic in
  ()*)
*)

let rec select_not_parsed_sentence = function
    RawSentence s -> false,RawSentence s
  | StructSentence _ -> failwith "select_not_parsed_sentence"
  | DepSentence _ -> failwith "select_not_parsed_sentence"
  | QuotedSentences sentences ->
      let l = Xlist.fold sentences [] (fun l p ->
        let b,sentence = select_not_parsed_sentence p.sentence in
        if b then {p with sentence=sentence} :: l else l) in
	  if l = [] then false,AltSentence [] else true, QuotedSentences(List.rev l)
  | AltSentence l -> 
      let b,l = Xlist.fold l (false,[]) (fun (b0,l) (mode,sentence) ->
        let b, sentence = select_not_parsed_sentence sentence in
		b0 || b, (mode,sentence) :: l) in
	  if b then true, AltSentence(List.rev l) else false, AltSentence []
  | ENIAMSentence result -> 
      if result.status = Parsed || result.status = PartialParsed || 
        result.status = SemParsed || result.status = PartialSemParsed || result.status = Inferenced 
	  then false,ENIAMSentence result else true,ENIAMSentence result
  | ErrorSentence s -> true,ErrorSentence s

let rec select_not_parsed_paragraph = function
    RawParagraph s -> false,RawParagraph s
  | StructParagraph sentences ->
      let l = Xlist.fold sentences [] (fun l p ->
        let b,sentence = select_not_parsed_sentence p.sentence in
        if b then {p with sentence=sentence} :: l else l) in
	  if l = [] then false,AltParagraph [] else true, StructParagraph(List.rev l)
  | AltParagraph l -> 
      let b,l = Xlist.fold l (false,[]) (fun (b0,l) (mode,paragraph) ->
        let b, paragraph = select_not_parsed_paragraph paragraph in
		b0 || b, (mode,paragraph) :: l) in
	  if b then true, AltParagraph(List.rev l) else false, AltParagraph []
  | ErrorParagraph s -> true,ErrorParagraph s

let rec select_not_parsed_text = function
    RawText s -> false,RawText s
  | StructText paragraphs ->
      let l = Xlist.fold paragraphs [] (fun l paragraph ->
        let b,paragraph = select_not_parsed_paragraph paragraph in
        if b then paragraph :: l else l) in
	  if l = [] then false,AltText [] else true,StructText(List.rev l)
  | JSONtext s -> true,JSONtext s
  | AltText l -> 
      let b,l = Xlist.fold l (false,[]) (fun (b0,l) (mode,text) ->
        let b, text = select_not_parsed_text text in
		b0 || b, (mode,text) :: l) in
	  if b then true, AltText(List.rev l) else false, AltText []
  | ErrorText s -> true,ErrorText s

let select_not_parsed text = 
  let b,text = select_not_parsed_text text in
  text


