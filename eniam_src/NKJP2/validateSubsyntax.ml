
open ENIAMtokenizerTypes
open Xstd

let space = {empty_token_env with orth=" "; token=Symbol " "}
let query_beg = {empty_token_env with token=Interp "<query>"}
let query_end = {empty_token_env with token=Interp "</query>"}
let sencence_beg = {empty_token_env with token=Interp "<sentence>"}
let sencence_end = {empty_token_env with token=Interp "</sentence>"}
let clause_beg = {empty_token_env with token=Interp "<clause>"}
let clause_end = {empty_token_env with token=Interp "</clause>"}

type sent = SentBeg | SentEnd | Inside | SentBegEnd

let set_sent_end = function
    (Inside,beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l,_ ->
      (SentEnd,beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l
  | (SentBeg,beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l,_ ->
      (SentBegEnd,beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l
  | _ -> failwith "set_sent_end"

let set_beg_as_zero = function
    (sent,_,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l ->
      (sent,0,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l
  | [] -> failwith "set_beg_as_zero"

let flatten_sentences sentences =
  List.rev (Xlist.fold sentences [] (fun l (id_s,tokens,named_tokens) ->
    set_sent_end (Xlist.fold tokens (l,SentBeg) (fun (l,sent) (beg,len,no_spaces,real_orth,orth,lemma,cat,interp) ->
      (sent,beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l, Inside))))

let make_token orth lemma cat interp =
  {empty_token_env with
         orth=orth;
         token=Lemma(lemma,cat,[Xlist.map interp (fun s -> [s])])}

let suffixes = StringSet.of_list ["by"; "ż"; "ń"; "że"; "%"; "BY"; "ś"; "li"; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ""; ]
(* let prefixes = StringSet.of_list [
  (*"\""; "-"; "("; "„"; "/"; "."; "+"; "«"; "''"; "»"; "["; "–"; "'";
  "’"; ":"; "“"; ","; ")";*) ""; ""; ""; ""; ""; ""; ] *)

let is_space_required prev_orth prev_cat orth cat =
  if cat = "interp" || cat = "aglt" || prev_cat = "interp" || prev_cat = "" || StringSet.mem suffixes orth then false else (
  let prev_char = List.hd (List.rev (Xunicode.classified_chars_of_utf8_string prev_orth)) in
  let cur_char = List.hd (Xunicode.classified_chars_of_utf8_string orth) in
  match prev_char,cur_char with
    Xunicode.Sign a,Xunicode.Sign b -> (*print_endline ("is_space_required 1: " ^ prev_orth ^ " " ^ orth ^ " " ^ a ^ " " ^ b);*) true
  | _,Xunicode.Sign _ -> false
  | Xunicode.Sign _,_ -> false
  | Xunicode.Digit _,Xunicode.Digit _ -> true
  | Xunicode.Digit _,_ -> false
  | _,Xunicode.Digit _ -> false
  | Xunicode.Small _,Xunicode.Small _ -> true
  | Xunicode.ForeignSmall _,Xunicode.Small _ -> true
  | Xunicode.Capital _,Xunicode.Capital _ -> true
  | Xunicode.Small _,Xunicode.Capital _ -> true
  | Xunicode.Capital _,Xunicode.Small _ -> true
  | Xunicode.ForeignCapital _,Xunicode.Small _ -> true
  | a,b -> failwith ("is_space_required: " ^ prev_orth ^ " " ^ orth ^ " " ^ Xunicode.to_string a ^ " " ^ Xunicode.to_string b))

let rec allign prev_orth prev_cat rev = function
    (SentBeg,0,_,_,_,orth,lemma,cat,interp) :: l ->
       allign orth cat ((make_token orth lemma cat interp) :: clause_beg :: sencence_beg :: query_beg :: rev) l
  | (SentBegEnd,0,_,_,_,orth,lemma,cat,interp) :: l ->
       allign orth cat (List.rev [query_beg;sencence_beg;clause_beg;make_token orth lemma cat interp;clause_end;sencence_end]) l
  | (_,0,_,_,_,orth,lemma,cat,interp) :: l -> failwith ("allign 1: " ^ orth)
  | (sent,beg,_,no_spaces,_,orth,lemma,cat,interp) :: l ->
       let rev =
         if no_spaces > 0 then space :: rev else
         if is_space_required prev_orth prev_cat orth cat then space :: rev else rev in
       if sent = SentBegEnd then
         let rev = (List.rev [sencence_beg;clause_beg;make_token orth lemma cat interp;clause_end;sencence_end]) @ rev in
         allign orth cat rev l
       else
       let rev = if sent = SentBeg then clause_beg :: sencence_beg :: rev else rev in
       let rev = (make_token orth lemma cat interp) :: rev in
       let rev = if sent = SentEnd then sencence_end :: clause_end :: rev else rev in
       allign orth cat rev l
  | [] -> List.rev (query_end :: rev)

let render_paragraph tokens =
  String.concat "" (List.rev (Xlist.rev_map tokens (fun t -> t.orth)))

let validate_render_paragraph name typ channel entries =
  prerr_endline name;
  Xlist.iter entries (fun (id_div,has_ne,paragraphs) ->
    Xlist.iter paragraphs (fun (paragraph,sentences) ->
      let tokens = flatten_sentences sentences in
      let tokens = allign "" "" [] (set_beg_as_zero tokens) in
      let rendered_paragraph = render_paragraph tokens in
      if paragraph <> rendered_paragraph then
      print_endline (paragraph ^ "\n" ^ rendered_paragraph)))

let count_subsyntax_errors stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let tokens = flatten_sentences sentences in
      let tokens = allign "" "" [] (set_beg_as_zero tokens) in
      let paragraph = render_paragraph tokens in
      let text,tokens,msg = ENIAMsubsyntax.catch_parse_text paragraph in
      let msg = if msg = "" then "Parsed" else msg in
      StringQMap.add stats (typ ^ " " ^ channel ^ " " ^ msg)))

let print_subsyntax_errors name typ channel entries =
  prerr_endline name;
  Xlist.iter entries (fun (id_div,has_ne,paragraphs) ->
    Xlist.iter paragraphs (fun (paragraph,sentences) ->
      (* Printf.printf "%d\t%s\n" id_div paragraph; *)
      let tokens = flatten_sentences sentences in
      let tokens = allign "" "" [] (set_beg_as_zero tokens) in
      let paragraph = render_paragraph tokens in
      (* Printf.printf "\t%s\n" paragraph; *)
      let text,tokens,msg = ENIAMsubsyntax.catch_parse_text paragraph in
      if msg <> "" then print_endline (msg ^ "\t" ^ paragraph)))

let validate stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      (* Printf.printf "%d\t%s\n" id_div paragraph; *)
      let tokens = flatten_sentences sentences in
      let tokens = allign "" "" [] (set_beg_as_zero tokens) in
      let paragraph = render_paragraph tokens in
      (* Printf.printf "\t%s\n" paragraph; *)
      let text,tokens,msg = ENIAMsubsyntax.catch_parse_text paragraph in
      let msg = if msg = "" then "Parsed" else msg in
      StringQMap.add stats (typ ^ " " ^ channel ^ " " ^ msg)))

let selection = StringSet.of_list ["200-4-000014";"040-2-000007";"120-2-900126";"120-2-910000001";"120-2-910000002";"120-4-900005";
"620-3-010001110";"620-3-010001449";"620-3-010001622";"620-3-010001727";
"620-3-010001731";"620-3-010001741";"620-3-010001854";"711-3-010000051";"711-3-010000056";
"711-3-010000079";"720-3-010000217";"720-3-010000335";"720-3-010000341";"forumowisko.pl_18535";"forumowisko.pl_424";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";]

let _ =
  ENIAMsubsyntax.initialize ();
  Gc.compact ();
  prerr_endline "Ready!";
  (* ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path () (fun () (name,typ,channel,entries) ->
    validate_render_paragraph name typ channel entries); *)
  let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    count_subsyntax_errors stats name typ channel entries) in
  (* ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path StringSet.empty ["publ"] ["prasa_dziennik"] () (fun () (name,typ,channel,entries) ->
    print_subsyntax_errors name typ channel entries); *)
  (* ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path () (fun () (name,typ,channel,entries) ->
    print_subsyntax_errors name typ channel entries); *)
  (* let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate stats name typ channel entries) in *)
  let stats = StringQMap.fold stats [] (fun stats k v -> (v,k) :: stats) in
  Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%d\t%s\n" v k);
  ()
