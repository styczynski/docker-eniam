open Xstd
open ENIAM_LCGlexiconTypes
open ENIAM_LCGtypes
open ENIAMsubsyntaxTypes

(* let _ =
  let path =
    try
      Sys.argv.(1)
    with
    | _ -> failwith ("Usage: " ^ Sys.argv.(0) ^ " corpus_name1 corpus_name2\n") in
  let corpus = File.file_in path (fun file -> CONLL.match_corpus (CONLL.load_corpus file)) in
  let sorted_corpus = List.sort Pervasives.compare corpus in
  List.iter (fun query -> match query with
      | AltText[Raw,RawText query;CONLL,StructText[
          StructParagraph[{sentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence [dep_paths]]} as p]]],tokens ->
            begin
              let str_query = CONLL.string_of_sentence_env CONLL tokens p in
              print_string str_query;
              flush stdout
            end
      | _ -> failwith "buuu") sorted_corpus *)

let _ =
  let path1 =
    try
      Sys.argv.(1)
    with
    | _ -> failwith ("Usage: " ^ Sys.argv.(0) ^ " corpus_name1 corpus_name2\n") in
  let path2 =
    try
      Sys.argv.(2)
    with
    | _ -> failwith ("Usage: " ^ Sys.argv.(0) ^ " corpus_name1 corpus_name2\n") in
  let corpus1 = File.file_in path1 (fun file -> CONLL.match_corpus (CONLL.load_corpus file)) in
  let corpus2 = File.file_in path2 (fun file -> CONLL.match_corpus (CONLL.load_corpus file)) in
  let filtered_corp = List.filter (fun (text1,tokens1) ->
              try
                let text2, tokens2 = List.find (fun (_,tokens2) -> CONLL.get_text tokens1 = CONLL.get_text tokens2) corpus2 in
                if text1 = text2
                then false (* jest w corpus2 - ma takie same drzewa *)
                else true (* jest w corpus2 - ma różne drzewa *)
              with
              | _ -> false (* nie ma w corpus2 *)
            ) corpus1 in
  let filtered_corp = List.map (fun (text1,tokens1) ->
                List.find (fun (_,tokens2) -> CONLL.get_text tokens1 = CONLL.get_text tokens2) corpus2) filtered_corp in
  List.iter (fun query -> match query with
      | AltText[Raw,RawText query;CONLL,StructText[
          StructParagraph[{sentence = AltSentence[Raw, RawSentence text; CONLL, DepSentence [dep_paths]]} as p]]],tokens ->
            begin
              let str_query = CONLL.string_of_sentence_env CONLL tokens p in
              print_string str_query;
              flush stdout
            end
      | _ -> failwith "buuu") filtered_corp
