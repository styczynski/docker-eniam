open Xstd

let render_sentence tokens =
  let tokens = match tokens with
      (beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l -> (beg,len,0,real_orth,orth,lemma,cat,interp) :: l
    | _ -> tokens in
  String.concat "" (Xlist.map tokens (fun (beg,len,no_spaces,real_orth,orth,lemma,cat,interp) ->
    (if no_spaces > 0 then " " else "") ^ real_orth))

let print_sentences name typ channel entries =
  prerr_endline name;
  Xlist.iter entries (fun (id_div,has_ne,paragraphs) ->
    Xlist.iter paragraphs (fun (paragraph,sentences) ->
      Xlist.iter sentences (fun (id_s,tokens,named_tokens) ->
        let token_string = String.concat " " (Xlist.map tokens (fun (beg,len,no_spaces,real_orth,orth,lemma,cat,interp) -> real_orth)) in
        Printf.printf "%s\t%s\t%s\t%d\t%s\t%s\n" typ channel name id_div id_s token_string)))

let find_sentences_skladnica not_found found name typ channel entries =
  prerr_endline name;
  Xlist.fold entries (not_found,found) (fun (not_found,found) (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs (not_found,found) (fun (not_found,found) (paragraph,sentences) ->
      Xlist.fold sentences (not_found,found) (fun (not_found,found) (id_s,tokens,named_tokens) ->
        let sentence = render_sentence tokens in
        (* print_endline sentence; *)
        if StringSet.mem not_found sentence then
          StringSet.remove not_found sentence,
          (name,typ,channel,id_div,id_s) :: found
        else not_found,found)))

let selection = StringSet.of_list [(*"200-4-000000316""130-3-900001"*)"310-4-000000004"]

let _ =
  prerr_endline "Ready!";
  (* ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path () (fun () (name,typ,channel,entries) ->
    print_sentences name typ channel entries); *)
  let sentences_skladnica = StringSet.of_list (File.load_lines "../../NLP resources/sentences-składnica.txt") in
  let not_found,found = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path (sentences_skladnica,[]) (fun (not_found,found) (name,typ,channel,entries) ->
    find_sentences_skladnica not_found found name typ channel entries) in
  (* let not_found,found = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection (sentences_skladnica,[]) (fun (not_found,found) (name,typ,channel,entries) ->
    find_sentences_skladnica not_found found name typ channel entries) in *)
  (* StringSet.iter not_found print_endline; *)
  let stats = Xlist.fold found StringQMap.empty (fun stats (name,typ,channel,id_div,id_s) ->
    StringQMap.add stats (typ ^ "\t" ^ channel)) in
  let stats = StringQMap.fold stats [] (fun stats k v -> (v,k) :: stats) in
  Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%d\t%s\n" v k);
  ()

(* Wyniki porównania sentences_składnica.txt:
   - niedopasowanych pozostało 177 zdań spośród 19957
   - przyczyny niedopasowania to
   -- różnica w podziale na zdania (rozdzielenie zdań przekraczających granicę akapitu)
   -- brak spacji w pierwotnym tekście, która jest dodana w sentences_składnica

Podział zdań ze względu na kategorie:
337     net_interakt    internet
172     net_nieinterakt internet
18      listy   ksiazka
155     nklas   ksiazka
225     publ    ksiazka
343     nd      ksiazka
845     inf-por ksiazka
1149    fakt    ksiazka
4817    lit     ksiazka
20      media   mowiony
70      konwers mowiony
2401    publ    prasa
5847    publ    prasa_dziennik
110     urzed   prasa_inne
268     publ    prasa_inne
445     qmow    prasa_inne
335     publ    prasa_miesiecznik
2050    publ    prasa_tygodnik *)
