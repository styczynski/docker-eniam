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

open Xstd
open Printf
open LCGtypes
open ExecTypes

let get_sock_addr host_name port =
  let he = Unix.gethostbyname host_name in
  let addr = he.Unix.h_addr_list in
  Unix.ADDR_INET(addr.(0),port)

let get_paths query =
  let i,o = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  Printf.fprintf o "%s\n%!" query;
  let paths,msg,time = (Marshal.from_channel i : ((int * int * ENIAMtokenizerTypes.token_record) list * int * int) * string * float) in
  Printf.fprintf o "\n%!";
  let _ = Unix.shutdown_connection i in
  paths,msg,time

(* FIXME: problem z "matka nie ufała im": dwukrotna interpretacji "im" *)

let simple_disambiguate (paths,last) =
  Xlist.fold paths [] (fun paths (i,j,t) ->
    if Xlist.mem t.ENIAMtokenizerTypes.attrs "notvalidated proper" || Xlist.mem t.ENIAMtokenizerTypes.attrs "lemma not validated" then paths else (i,j,t) :: paths),last

(* FIXME: przerobić koordynację *)

let lcg_process query =
  print_endline "lcg_process 1";
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  print_endline "lcg_process 2";
  let result = Exec.process_query ic oc 30. false "x" (ENIAMsubsyntaxTypes.RawText query,ExtArray.make 1 ENIAMtokenizerTypes.empty_token) 10 in
  print_endline "lcg_process 3";
  let path = "results/" in
  Visualization.print_html_text path "input_text" result.input_text result.tokens result.lex_sems;
  Visualization.print_html_text path "pre_text" result.pre_text result.tokens result.lex_sems;
  Visualization.print_html_text path "parsed_text" result.parsed_text result.tokens result.lex_sems;
  Visualization.print_html_text path "selected_sent_text" result.selected_sent_text result.tokens result.lex_sems;
  Visualization.print_html_text path "semantic_text" result.semantic_text result.tokens result.lex_sems;
  Visualization.print_html_text path "selected_semantic_text" result.selected_semantic_text result.tokens result.lex_sems;
  Visualization.print_main_result_text "aaa/" (path ^ "main/") "xxxx" result.tokens result.selected_semantic_text;
  Exec.print_result stdout result;
  (*Visualization.print_paths "results/" "paths" result.paths;
  Visualization.print_paths_latex "paths" result.paths;
  (match result.status with
    Parsed ->
(*        LCGreductions.print_references "chart" result.term;  *)
(*        Visualization.print_tree "results/" "tree1" result.paths result.term;  *)
(*        Visualization.print_tree "results/" "tree2" result.paths result.disamb;  *)
(*       Visualization.print_graph "results/" "term1" result.term;
       Visualization.print_graph "results/" "term2" result.disamb;*)
       Visualization.print_graph "results/" "term3" result.sem;
       Visualization.print_graph "results/" "term4" result.sem2;
        Visualization.print_graph2 "results/" "term5" query result.sem3;
(*       Visualization.print_xml_graph "results/" "graph" result.term; *)
(*      LatexMain.latex_file_out "results/" "chart" "a0" false (fun file ->
        Int.iter 0 (Array.length result.sem - 1) (fun i ->
          Printf.fprintf file "%s\n" (LCGchart.latex_of_linear_term 0 result.sem.(i))));*)
(*         Printf.fprintf file "$%s$\n\n" (LCGchart.latex_of_linear_term_simple 0 result.sem); *)
(*         Printf.fprintf file "$%s$\n" (LCGchart.latex_of_linear_term 0 result.sem)); *)
(*       LatexMain.latex_compile_and_clean "results/" "chart" *)
  let path = "results/web/" in
  ignore(Xlist.fold2 result.trees result.mrls 1 (fun n tree mrl ->
      Visualization.print_graph2 path ("tree_" ^ string_of_int n) "" tree;
      Visualization.print_xml_tree path ("tree_" ^ string_of_int n) tree;
      let mml = SemMmlOf.mml_of_mrl mrl in
      Visualization.print_mml path ("formula_" ^ string_of_int n) mml;
      n+1));
(*     ignore(Xlist.fold result.trees 1 (fun n tree ->
       Visualization.print_graph2 "results/" ("tree_" ^ string_of_int n) query tree;
       n+1));
     SemLatexOf.print_mrls_latex "results/" "result" query result.mrls;*)
     ()
  | NotTranslated ->
(*        LCGreductions.print_references "chart" result.term;  *)
(*        Visualization.print_tree "results/" "tree1" result.paths result.term;  *)
(*        Visualization.print_tree "results/" "tree2" result.paths result.disamb;  *)
(*       Visualization.print_graph "results/" "term1" result.term;
       Visualization.print_graph "results/" "term2" result.disamb;*)
       Visualization.print_graph "results/" "term3" result.sem;
       Visualization.print_graph "results/" "term4" result.sem2;
        Visualization.print_graph2 "results/" "term5" query result.sem3;
(*       Visualization.print_xml_graph "results/" "graph" result.term; *)
      ()
  | SemError ->
       Visualization.print_graph "results/" "term1" result.term;
       Visualization.print_graph "results/" "term2" result.disamb;
       Visualization.print_graph "results/" "term3" result.sem;
       Visualization.print_graph "results/" "term4" result.sem2;
        Visualization.print_graph2 "results/" "term5" query result.sem3;
  | NotParsed ->
      LatexMain.latex_file_out "results/" "chart" "a1" false (fun file ->
        Printf.fprintf file "%s\n" (LCGlatexOf.graph result.graph));
      LatexMain.latex_compile_and_clean "results/" "chart"
  | NotReduced ->
      LCGlatexOf.print_references "chart" result.term
(*      LatexMain.latex_file_out "results/" "chart" "a0" false (fun file ->
        Int.iter 0 (Array.length result.sem - 1) (fun i ->
          Printf.fprintf file "%s\n" (LCGchart.latex_of_linear_term 0 result.sem.(i))));
(*         Printf.fprintf file "$%s$\n\n" (LCGchart.latex_of_linear_term_simple 0 result.sem); *)
(*         Printf.fprintf file "$%s$\n" (LCGchart.latex_of_linear_term 0 result.sem)); *)
      LatexMain.latex_compile_and_clean "results/" "chart"*)
  | _ -> ());*)
  (* Printf.fprintf oc "\n%!"; *)
  Marshal.to_channel oc (ENIAMsubsyntaxTypes.RawText "",ExtArray.make 1 ENIAMtokenizerTypes.empty_token) [];
  flush oc;
  let _ = Unix.shutdown_connection ic in
  ()

let _ =
  if Array.length Sys.argv < 2 then print_endline "missing argument" else
  lcg_process Sys.argv.(1)

(* FIXME: parser dziwnie się zachowuje dla 'ścieżki anomalia.' 'ścieżki anomalia. GG' itp. - nie parsuje '.' a jak sparsuje to nie chce redukować *)
(* FIXME!!!: w zdaniu "Stokrotka faksowała", 'faksować' ma 20 schematów, większość z nich po ograniczeniu jest identyczna i daje te same semantyki, trzeba to jakoś zdezambiguować *)

(* FIXME:
Jestem.: brak podmiotu
Będę.: brak podmiotu
Bym frunął.: unk Bym
Powinienby.: unk
Powinienbyś.: unk
Boisz się.	się w leksemie
Myję się.	refl
Myję siebie.	siebie
*)

let lcg_process_file filename result_path result_name = failwith "lcg_process_file"
  (*let sentences = File.load_lines filename in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  let id = ref 0 in
  Xlist.iter sentences (fun query ->
    incr id;
    let query = List.hd (Str.split (Str.regexp "\t") query) in
    print_endline query;
    let result = Exec.process_query ic oc 3000. false "x" (PreTypes.RawText query) 10 in
(*     LCGexec.print_result stdout result; *)
    if result.status = Parsed then
      Visualization.print_graph2 result_path (result_name ^ string_of_int !id) query result.sem3)*)

(* let _ = lcg_process_file "data/testy_podstawowe_rob.txt" "results/testy_podstawowe/" "test"   *)
(* let _ = lcg_process_file "data/zdania_testowe.txt" "zdania_testowe"    *)

(* let _ = Exec.test_process_file "data/testy_podstawowe.txt" "results/testy_podstawowe.eff" 100.    *)
(* let _ = LCGexec.test_process_file "data/sentences-składnica.txt" "results/sentences-składnica.eff" 100. *)
(* let _ = LCGexec.process_file_id "data/sentences-składnica-with-trees.tab" "results/sentences-składnica-with-trees.eff" 100. *)

(* Przetwarzanie korpusów w formacie CONLL *)
(*
let id_counter = ref 0

let get_id () =
  incr id_counter;
  "ID_" ^ (string_of_int !id_counter)

let get_query_id = function
    PreTypes.AltText[_;PreTypes.CONLL,PreTypes.StructText([PreTypes.StructParagraph[p]],_)] -> if p.PreTypes.pid = "" then get_id () else p.PreTypes.pid
  | PreTypes.AltText[PreTypes.CONLL,PreTypes.StructText([PreTypes.StructParagraph[p]],_)] -> if p.PreTypes.pid = "" then get_id () else p.PreTypes.pid
  | _ -> failwith "get_query_id"

let get_query_text = function
    PreTypes.AltText[PreTypes.Raw,PreTypes.RawText text;_] -> text
  | _ -> failwith "get_query_text"

let process_id s =
  if Xstring.check_prefix "ID_" s then s else
  let a,b,c = match Xstring.split_delim "/" s with
      [a;b;c] -> a,b,c
    | _ -> failwith ("process_id: " ^ s) in
  if Xstring.check_prefix "NKJP_1M_" a && Xstring.check_prefix "morph_" b && Xstring.check_sufix "-p" b &&
     Xstring.check_prefix "morph_" c && Xstring.check_sufix "-s" c then
    Xstring.cut_prefix "NKJP_1M_" a ^ "." ^ Xstring.cut_sufix "-s" (Xstring.cut_prefix "morph_" c)
  else failwith ("process_id: " ^ s)
*)
(* FIXME
let process_conll_corpus filename =
  let corpus = File.file_in filename (fun file -> CONLL.match_corpus (CONLL.load_corpus file)) in
  print_endline "process_conll_corpus";
  let corpus = [List.hd corpus] in
  let ic,oc = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  Xlist.iter corpus (fun query ->
    let id = process_id (get_query_id query) in
    let path = "results/" ^ id ^ "/" in
    ignore (Sys.command ("mkdir -p " ^ path));
    let result = Exec.process_query ic oc 30. false "x" query 10 in
    Visualization.print_html_text path "input_text" result.input_text;
    Visualization.print_html_text path "pre_text" result.pre_text;
    Visualization.print_html_text path "parsed_text" result.parsed_text;
    Visualization.print_html_text path "selected_sent_text" result.selected_sent_text;
    Visualization.print_html_text path "semantic_text" result.semantic_text;
    Visualization.print_html_text path "selected_semantic_text" result.selected_semantic_text;
    (* printf "input_text:\n%s\n" (Visualization.string_of_text result.input_text);
    printf "pre_text:\n%s\n" (Visualization.string_of_text result.pre_text); *)
    (* Exec.print_result stdout result; *)
    LCGfields.print_fields ["arole"] result.parsed_text;
    (* CompTrees.compare_results result.parsed_text; *)
    (* Visualization.print_paths "results/" "paths" result.paths; *)
    ());
  Marshal.to_channel oc (PreTypes.RawText "",ExtArray.make 1 ENIAMtokenizerTypes.empty_token) [];
  flush oc;
  let _ = Unix.shutdown_connection ic in
  ()
*)
(*let _ =
  (* process_conll_corpus "../../NLP resources/Skladnica-zaleznosciowa-mod_130121.conll"; *)
  (* process_conll_corpus "../../NLP resources/skladnica_zaleznosciowa.conll"; *)
  process_conll_corpus "../testy/skladnica-test1.conll";
  ()*)

  (* TO DO:
  - współbieżne uruchamianie parserów
  - nkjp jako źródło danych
  - concraft, wcrft2
  - dopasowanie do siebie tokenów w różnych wersjach tokenizacji
  2016.10.15
  - nadmiar węzłów pro po parsowaniu
  2016.10.16
  - sprawdzenie zerowania globalnych referencji przy parsowaniu korpusu
  2016.10.22
  - instalacja Świgry
  *)


(*
let has_pos pos (paths,_,_) =
  Xlist.fold paths false (fun b (_,_,t) ->
    match t.PreTypes.token with
      PreTypes.Lemma(_,cat,_) -> if cat = pos then true else b
    | _ -> b)
*)
(* Wydobycie zdań zawierających symbole *)
(*let _ =
  let i,o = Unix.open_connection (get_sock_addr host port) in
  let _ = NKJPtext.fold_text Paths.nkjp 1 (fun n (name,id_div,ab_list) ->
    if n mod 50 = 0 then Printf.printf "COUNT=%d\n%!"n;
    Xlist.iter ab_list (fun (id_ab,query) ->
      Printf.fprintf o "%s\n%!" query;
      let paths,msg,pre_time = (Marshal.from_channel i : ((int * int * PreTypes.token_record) list * int * int) * string * float) in
      if msg <> "" then Printf.printf "ERROR: %s %s %s:\nERROR: %s\nERROR: %s\n%!" name id_div id_ab msg query;
      if has_pos "email" paths then Printf.printf "%s\n%!" query);
    n+1) in
  Printf.fprintf o "\n%!";
  let _ = Unix.shutdown_connection i in
  ()*)

(* Wydobycie zdań zawierających symbole *)
(*let _ =
  let sentences = File.load_lines "data/NKJP1M_symbols.txt" in
  let i,o = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  let _ = Xlist.fold sentences 0 (fun n query ->
    if n mod 100 = 0 then Printf.printf "COUNT=%d\n%!"n;
    Printf.fprintf o "%s\n%!" query;
    let paths,msg,pre_time = (Marshal.from_channel i : ((int * int * PreTypes.token_record) list * int * int) * string * float) in
    if msg <> "" then Printf.printf "ERROR: %s\nERROR: %s\n%!" msg query;
    if has_pos "obj-id" paths then Printf.printf "%s\n%!" query;
    n+1) in
  Printf.fprintf o "\n%!";
  let _ = Unix.shutdown_connection i in
  ()*)

(*let _ =
  let i,o = Unix.open_connection (get_sock_addr host port) in
  let _ = NKJPtext.fold_text Paths.nkjp 1 (fun n (name,id_div,ab_list) ->
    if n mod 50 = 0 then Printf.printf "%d\n%!"n;
    Xlist.iter ab_list (fun (id_ab,query) ->
      Printf.fprintf o "%s\n%!" query;
      let paths,msg,pre_time = (Marshal.from_channel i : ((int * int * PreTypes.token_record) list * int) * string * float) in
      if msg <> "" then Printf.printf "%s %s %s:\n%s\n%s\n%!" name id_div id_ab msg query);
    n+1) in
  Printf.fprintf o "\n%!";
  let _ = Unix.shutdown_connection i in
  ()*)

(* Test poprawności i szybkości pre *)
(*let _ =
  let sentences = File.load_lines "data/sentences-składnica.txt" in
  let i,o = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  let utime = Unix.gettimeofday () in
  let n,pre_time = Xlist.fold sentences (0,0.) (fun (n,time) query ->
    if n mod 100 = 0 then Printf.printf "%d\n%!"n;
    Printf.fprintf o "%s\n%!" query;
    let paths,msg,pre_time = (Marshal.from_channel i : ((int * int * PreTypes.token_record) list * int) * string * float) in
    if msg <> "" then Printf.printf "%s\n%!" query;
    n+1,time+.pre_time) in
  let utime2 = Unix.gettimeofday () in
  Printf.fprintf o "\n%!";
  let _ = Unix.shutdown_connection i in
  Printf.printf "pre_time=%f (avg=%f) utime=%f (avg=%f)\n" pre_time (pre_time /. float n) (utime2-.utime) ((utime2-.utime) /. float n);
  ()*)


let simplify_pos = function
    "subst" -> "noun"
  | "depr" -> "noun"
  | "adj" -> "adj"
  | "adja" -> "adj"
  | "adjc" -> "adj"
  | "adjp" -> "adj"
  | "ger" -> "verb"
  | "pact" -> "verb"
  | "ppas" -> "verb"
  | "fin" -> "verb"
  | "bedzie" -> "verb"
  | "praet" -> "verb"
  | "winien" -> "verb"
  | "impt" -> "verb"
  | "imps" -> "verb"
  | "inf" -> "verb"
  | "pcon" -> "verb"
  | "pant" -> "verb"
  | "pred" -> "verb"
  | s -> s

type stats = {noun: int; noun_sense: int; noun_valence: int;
              verb: int; verb_sense: int; verb_valence: int;
              adj: int; adj_sense: int; adj_valence: int;
              }

let empty_stats = {noun=0; noun_sense=0; noun_valence=0;
                   verb=0; verb_sense=0; verb_valence=0;
                   adj=0; adj_sense=0; adj_valence=0;
                   }

let print_stats n stats =
  Printf.printf "noun=%d (avg=%f) noun_sense=%d (avg=%f, %f) noun_valence=%d (avg=%f, %f)\n"
    stats.noun (float stats.noun /. float n)
    stats.noun_sense (float stats.noun_sense /. float n) (float stats.noun_sense /. float stats.noun)
    stats.noun_valence (float stats.noun_valence /. float n) (float stats.noun_valence /. float stats.noun);
 Printf.printf "verb=%d (avg=%f) verb_sense=%d (avg=%f, %f) verb_valence=%d (avg=%f, %f)\n"
    stats.verb (float stats.verb /. float n)
    stats.verb_sense (float stats.verb_sense /. float n) (float stats.verb_sense /. float stats.verb)
    stats.verb_valence (float stats.verb_valence /. float n) (float stats.verb_valence /. float stats.verb);
  Printf.printf "adj=%d (avg=%f) adj_sense=%d (avg=%f, %f) adj_valence=%d (avg=%f, %f)\n"
    stats.adj (float stats.adj /. float n)
    stats.adj_sense (float stats.adj_sense /. float n) (float stats.adj_sense /. float stats.adj)
    stats.adj_valence (float stats.adj_valence /. float n) (float stats.adj_valence /. float stats.adj);
  ()
(*
let get_stats stats (paths,_) =
  Xlist.fold paths stats (fun stats (_,_,t) ->
(*     if Xlist.mem t.PreTypes.attrs "notvalidated proper" || Xlist.mem t.PreTypes.attrs "lemma not validated" then stats else *)
    match t.PreTypes.token with
      PreTypes.Lemma(_,pos,_) ->
         (match simplify_pos pos with
            "noun" -> {stats with noun=stats.noun+1;
                                  noun_sense=if t.PreTypes.senses=[] then stats.noun_sense else stats.noun_sense+1;
                                  noun_valence=if t.PreTypes.valence=[] then stats.noun_valence else stats.noun_valence+1}
          | "verb" -> {stats with verb=stats.verb+1;
                                  verb_sense=if t.PreTypes.senses=[] then stats.verb_sense else stats.verb_sense+1;
                                  verb_valence=if t.PreTypes.valence=[] then stats.verb_valence else stats.verb_valence+1}
          | "adj" -> {stats with adj=stats.adj+1;
                                 adj_sense=if t.PreTypes.senses=[] then stats.adj_sense else stats.adj_sense+1;
                                  adj_valence=if t.PreTypes.valence=[] then stats.adj_valence else stats.adj_valence+1}
          | _ -> stats)
    | _ -> stats)
*)
(* Test pokrycia słowosieci i walentego *)
(*let _ =
  let sentences = File.load_lines "data/sentences-składnica.txt" in
  let i,o = Unix.open_connection (get_sock_addr Paths.pre_host Paths.pre_port) in
  let n,stats = Xlist.fold sentences (0,empty_stats) (fun (n,stats) query ->
    if n mod 100 = 0 then Printf.printf "%d\n%!"n;
    Printf.fprintf o "%s\n%!" query;
    let paths,msg,pre_time = (Marshal.from_channel i : ((int * int * PreTypes.token_record) list * int) * string * float) in
    if msg <> "" then Printf.printf "%s\n%!" query;
    let stats = get_stats stats paths in
    n+1,stats) in
  Printf.fprintf o "\n%!";
  let _ = Unix.shutdown_connection i in
  print_stats n stats;
  ()*)

(*let _ =
  NKJPtext.fold_text Paths.nkjp () (fun () (name,id_div,ab_list) ->
    printf "%s %s:" name id_div;
    Xlist.iter ab_list (fun (id_ab,s) -> printf " %s" id_ab);
    printf "\n")*)

(**let rec get_shortest n found = function
    [] -> found
  | path :: paths ->
      if Xlist.size path > n then get_shortest n found paths
      else if Xlist.size path = n then get_shortest n (path :: found) paths
      else get_shortest (Xlist.size path) [path] paths

(*let simplify_paths paths =
  Xlist.map paths (fun path ->
    Xlist.map path (fun symbols ->
      StringSet.to_list (Xlist.fold symbols StringSet.empty (fun set (symbol,_) ->
        StringSet.add set (LCGchart.string_of_grammar_symbol 0 (LCGchart.simplify_symbol symbol))))))*)**)


(**let lcg_select_sentences filename output_filename =
  let sentences = File.load_lines filename in
  File.file_out output_filename (fun file ->
  Xlist.iter sentences (fun query ->
    try
      let paths = PreProcessing.parse query in
      if PrePaths.no_possible_path paths then
        Printf.fprintf file "%s\n%!" query
      else
      let paths = PrePaths.topological_sort paths in
      let graph,n = LCGlexicon.create query paths in
      let graph = LCGparser.parse "0" graph in
      if LCGpostprocessing.is_parsed graph n then ()
      else Printf.fprintf file "%s\n%!"  query
    with e -> Printf.fprintf file "%s\n%!" query));
  ()

(* let _ = lcg_select_sentences "data/sentences-składnica2.txt" "data/sentences-składnica-sel.txt" *)
**)
