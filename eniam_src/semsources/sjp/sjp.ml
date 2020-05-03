open Xstd
open Printf

let split_haslo s =
  match Xstring.split_delim "\xC2\xA0 " s with
    [lemma] -> lemma,""
  | [lemma;"I"] -> lemma,"I"
  | [lemma;"II"] -> lemma,"II"
  | [lemma;"III"] -> lemma,"III"
  | [lemma;"IV"] -> lemma,"IV"
  | [lemma;"V"] -> lemma,"V"
  | [lemma;"VI"] -> lemma,"VI"
  | _ -> failwith ("split_haslo: " ^ s)

let rec check_cut_prefix pats s msg =
  match pats with
    [] -> failwith (msg ^ ": " ^ s)
  | pat :: pats ->
       if Xstring.check_prefix pat s then
         Xstring.cut_prefix pat s else
       check_cut_prefix pats s msg

let rec check_cut_sufix pats s msg =
  match pats with
    [] -> failwith (msg ^ ": " ^ s)
  | pat :: pats ->
       if Xstring.check_sufix pat s then
         Xstring.cut_sufix pat s else
       check_cut_sufix pats s msg

let full_split pat s =
  Xlist.map (Str.full_split (Str.regexp pat) s) (function
        Str.Text s -> s
      | Str.Delim s -> s)

type sense = {variant: string; qualifier: string; subsenses: (string * string * string list) list; see: string * string * string * string list}

type entry = {id: string; head: string; head_variant: string; pos: string; titles: (string * string) list; senses: sense list; derivatives: string list; pron: string}

let empty_sense = {variant=""; qualifier=""; subsenses=[]; see="","","",[]}
let empty_entry = {id=""; head=""; head_variant=""; pos=""; titles=[]; senses=[]; derivatives=[]; pron=""}

let parse_sense attrs l =
  let sense = Xlist.fold attrs empty_sense (fun sense -> function
      "variant",variant -> {sense with variant=variant}
    | "qualifier",qualifier -> {sense with qualifier=qualifier}
    | k,v -> failwith ("parse_sense 1: " ^ k ^ "=" ^ v)) in
  let sense = Xlist.fold l sense (fun sense -> function
      Xml.Element("subsense",attrs,Xml.PCData s :: l) ->
          let v = match attrs with
              [] -> ""
            | ["variant",v] -> v
            | _ -> failwith "parse_sense 6" in
          let l = Xlist.map l (function
              Xml.Element("example",[],[Xml.PCData s]) -> s
            | _ -> failwith "parse_sense 2") in
          {sense with subsenses=(v,s,l) :: sense.subsenses}
    | Xml.Element("see",attrs,l) ->
          let id,head,head_variant = Xlist.fold attrs ("","","") (fun (id,head,head_variant) -> function
              "id",id -> id,head,head_variant
            | "head",head -> id,head,head_variant
            | "head_variant",head_variant -> id,head,head_variant
            | k,v -> failwith ("parse_sense 3: " ^ k ^ "=" ^ v)) in
          let variants = Xlist.map l (function
              Xml.Element("sense",["variant",v],[]) -> v
            | _ -> failwith "parse_sense 4") in
          {sense with see=id,head,head_variant,variants}
    | _ -> failwith "parse_sense 5") in
  {sense with subsenses=List.rev sense.subsenses}

let load_sjp filename =
  match Xml.parse_file filename with
    Xml.Element("sjp",[],l) ->
      Xlist.rev_map l (function
        Xml.Element("entry",attrs,l) ->
          let entry = Xlist.fold attrs empty_entry (fun entry -> function
              "id",id -> {entry with id=id}
            | "head",head -> {entry with head=head}
            | "head_variant",head_variant -> {entry with head_variant=head_variant}
            | "pos",pos -> {entry with pos=pos}
            | k,v -> failwith ("load_sjp 2: " ^ k ^ "=" ^ v)) in
          let entry = Xlist.fold l entry (fun entry -> function
              Xml.Element("title",[],[Xml.PCData s]) -> {entry with titles=(s,"") :: entry.titles}
            | Xml.Element("title",["title_variant",v],[Xml.PCData s]) -> {entry with titles=(s,v) :: entry.titles}
            | Xml.Element("sense",attrs,l) -> {entry with senses=(parse_sense attrs l) :: entry.senses}
            | Xml.Element("der",[],[Xml.PCData s]) -> {entry with derivatives=s :: entry.derivatives}
            | Xml.Element("pron",[],l) -> {entry with pron=String.concat "" (Xlist.map l Xml.to_string)}
            | xml -> failwith ("load_sjp 3: " ^ Xml.to_string xml)) in
          {entry with titles=List.rev entry.titles; senses=List.rev entry.senses; derivatives=List.rev entry.derivatives}
      | _ -> failwith "load_sjp 4")
  | _ -> failwith "load_sjp 1"

let rec extract_words_rec = function
    "„" :: s :: "”" :: l -> s :: (extract_words_rec l)
  | "„" :: _ -> failwith "extract_words_rec"
  | "”" :: _ -> failwith "extract_words_rec"
  | s :: l -> extract_words_rec l
  | [] -> []

let extract_words s =
  extract_words_rec (full_split "„\\|”" s)

let find_words sjp =
  Xlist.fold sjp (StringSet.empty,StringSet.empty) (fun (words,ids) entry ->
    (* if entry.derivatives <> [] then Printf.printf "%s: %s\n" entry.head (String.concat " " entry.derivatives); *)
    let words = Xlist.fold entry.titles words (fun words (s,_) -> StringSet.add words s) in
    Xlist.fold entry.senses (words,ids) (fun (words,ids) sense ->
      let id,head,_,_ = sense.see in
      let ids = if id = "" then ids else StringSet.add ids id in
      let words = if head = "" then words else StringSet.add words head in
      let words = Xlist.fold sense.subsenses words (fun words (_,s,_) ->
        let l = extract_words s in
        Xlist.fold l words StringSet.add) in
      words,ids))

let id_list dir =
  let l = Array.to_list (Sys.readdir dir) in
  Xlist.fold l [] (fun l name ->
    if name = "lista" then l else
    (check_cut_sufix [".html"] name "id_list") :: l)

let html_header =
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<html>
  <head>
	<META HTTP-EQUIV=\"CONTENT-TYPE\" CONTENT=\"text/html; charset=utf8\">
	<TITLE>Słownik</TITLE>
	<META HTTP-EQUIV=\"Content-Language\" CONTENT=\"pl\">
  </head>

  <body>
 <center>"

let html_trailer =
"</center>
  </body>
</html>"

(* wygenerowanie listy adresów leksemów do których są odnośniki, a których nie ma lokalnie *)
(* let _ =
  let sjp = load_sjp "data/sjp.xml" in
  let words,ids = find_words sjp in
  Printf.printf "|words|=%d |ids|=%d\n%!" (StringSet.size words) (StringSet.size ids);
  let l = File.load_tab "data/adresy_hasla.tab" (function [name;url] -> name,url | _ -> failwith "load adresy_hasla") in
  let l = Xlist.fold l [] (fun l (name,url) ->
    let word,_ = split_haslo name in
    let id = check_cut_sufix [".html"] (check_cut_prefix ["http://sjp.pwn.pl/sjp/"] url "make id") "make id" in
    if StringSet.mem words word || StringSet.mem ids id then id :: l else l) in
  Printf.printf "|l|=%d\n%!" (Xlist.size l);
  let known_ids = StringSet.of_list (id_list "data/sjp.pwn.pl/sjp/") in
  Printf.printf "|known_ids|=%d\n%!" (StringSet.size known_ids);
  Xlist.iter l (fun id ->
    if StringSet.mem known_ids id then () else
    print_endline ("http://sjp.pwn.pl/sjp/" ^ id ^ ".html"));
  () *)

let html_of_subsenses sv l =
  String.concat "<br>" (List.flatten (Xlist.map l (fun (v,gloss,exs) ->
    let v = match v with
      "" -> ""
    | "a" -> "<b>a</b>"
    | _ -> (if sv = "" then "" else "&nbsp;&nbsp;&nbsp;") ^ "<b>"^v^"</b>" in
    (v^" "^gloss) :: (Xlist.map exs (fun ex -> "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<i>" ^ ex ^ "</i>")))))

let html_of_see (id,head,head_variant,variants) =
  let s = if variants = [] then "" else " w znaczeniu " ^ String.concat ", " variants in
  sprintf "<b>zobacz</b> „%s%s” %s" head (if head_variant = "" then "" else " " ^ head_variant) s

let html_of_entry e =
  let lemmas = String.concat "<br>" (Xlist.map e.titles (fun (s,v) -> if v = "" then s else s ^ " " ^ v)) in
  let head = "<b>" ^ (if e.head_variant = "" then e.head else e.head ^ " " ^ e.head_variant) ^ "</b>" in
  let lemmas = if lemmas = "" then head else head ^ "<br>" ^ lemmas in
  let senses = String.concat "<br>" (Xlist.map e.senses (fun s ->
    sprintf "<b>%s</b> <i>%s</i> %s" s.variant s.qualifier
      (if s.subsenses <> [] && s.see = ("","","",[]) then html_of_subsenses s.variant s.subsenses else
       if s.subsenses = [] && s.see <> ("","","",[]) then html_of_see s.see else
       failwith "html_of_entry"))) in
  sprintf "<td>%s</td><td>%s</td><td>%s</td>" lemmas e.pos senses

let html_of_entries filename l =
  File.file_out filename (fun file ->
    fprintf file "%s\n" html_header;
    fprintf file "<table border=1>\n";
    Xlist.iter l (fun e -> fprintf file "<tr>%s</tr>\n" (html_of_entry e));
    fprintf file "</table>\n";
    fprintf file "%s\n" html_trailer)

let clean_titles e =
  let titles = Xlist.fold e.titles [] (fun titles (t,v) ->
    if t = e.head && v = e.head_variant then titles else (t,v) :: titles) in
  {e with titles = List.rev titles}

let sjp_compare e f =
  if e.head <> f.head then compare e.head f.head else
  compare e.head_variant f.head_variant

let sjp_compare2 e f =
  let s = List.hd e.senses in
  let t = List.hd f.senses in
  if s.see <> t.see then compare s.see t.see else
  compare s.subsenses t.subsenses

let select_entries set sjp =
  Xlist.fold sjp [] (fun sjp e ->
    let b = Xlist.fold (e.head :: (Xlist.rev_map e.titles fst)) false (fun b s ->
      StringSet.mem set s || b) in
    if b then e :: sjp else sjp)

let load_lu_list filename =
  File.fold_tab filename StringMap.empty (fun map -> function
      [name;cat] -> StringMap.add_inc map name (StringSet.singleton cat) (fun set -> StringSet.add set cat)
    | l -> failwith ("load_lu_list: " ^ String.concat "\t" l))

let select_lu lu_map l =
  StringMap.fold lu_map StringSet.empty (fun set s cats ->
    let b = Xlist.fold l false (fun b cat -> StringSet.mem cats cat || b) in
    if b then StringSet.add set s else set)

let html_of_selected_entries sjp lu_map selected_cats filename =
  let set = select_lu lu_map selected_cats in
  let sjp = select_entries set sjp in
  let sjp = List.sort sjp_compare (Xlist.rev_map sjp clean_titles) in
  html_of_entries filename sjp

let _ =
  let lu_map = load_lu_list "../base_LU.tab" in
  let sjp = load_sjp "data/SJP.xml" in
(*  html_of_selected_entries sjp lu_map ["adv"] "adv.html";
  html_of_selected_entries sjp lu_map ["adj"] "adj.html";
  html_of_selected_entries sjp lu_map ["subst"] "subst.html";
  html_of_selected_entries sjp lu_map ["comp";"comp-NKJP1"] "comp.html";
  html_of_selected_entries sjp lu_map ["conj";"conj-NKJP1"] "conj.html";
  html_of_selected_entries sjp lu_map ["interj";"interj-NKJP1"] "interj.html";
  html_of_selected_entries sjp lu_map ["num";"num-NKJP1"] "num.html";
  html_of_selected_entries sjp lu_map ["prep";"prep-NKJP1"] "prep.html";
  html_of_selected_entries sjp lu_map ["qub";"qub-NKJP1"] "qub.html";
  html_of_selected_entries sjp lu_map ["burk";"burk-NKJP1"] "burk.html";*)
  (* let sjp = List.sort sjp_compare2 (Xlist.rev_map sjp clean_titles) in
  html_of_entries "sjp2.html" sjp; *)
  let sjp = List.sort sjp_compare (Xlist.rev_map sjp clean_titles) in
  html_of_entries "results/SJP.html" sjp;
  ()

let resolve_titles sjp =
  let sjp_set = Xlist.fold sjp StringSet.empty (fun set e ->
    StringSet.add set (e.head ^ "#" ^ e.head_variant)) in
  let titles = Xlist.fold sjp [] (fun titles e ->
    let e = clean_titles e in
    Xlist.fold e.titles titles (fun titles (title,title_variant) ->
      if StringSet.mem sjp_set (title ^ "#" ^ title_variant) then titles else
      (title,title_variant,e.head,e.head_variant) :: titles)) in
  Xlist.fold titles sjp (fun sjp (title,title_variant,head,head_variant) ->
    {empty_entry with head=title; head_variant=title_variant;
      senses=[{empty_sense with see="",head,head_variant,[]}]} :: sjp)

let assign_pos sjp =
  let sjp_map = Xlist.fold sjp StringMap.empty (fun map e ->
    StringMap.add map (e.head ^ "#" ^ e.head_variant) e.pos) in
  Xlist.rev_map sjp (fun e ->
    let pos_set = StringSet.singleton e.pos in
    let pos_set = Xlist.fold e.senses pos_set (fun pos_set s ->
      let _,head,head_variant,_ = s.see in
      try
        StringSet.add pos_set (StringMap.find sjp_map (head ^ "#" ^ head_variant))
      with Not_found -> pos_set) in
    let pos = String.concat "_" (StringSet.to_list (StringSet.remove pos_set "")) in
    {e with pos=pos})

let print_entry_sense file e s =
  let _,sh,sv,sl = s.see in
  if sh = "" then
    Xlist.iter s.subsenses (fun (v,gloss,ex) ->
      fprintf file "%s\t%s\t%s\t%s\t%s\t%s\t%s\n"
        e.head e.pos e.head_variant s.variant v
        gloss (String.concat " " ex))
  else
    let sl = if sl = [] then "" else " w znaczeniu " ^ String.concat ", " sl in
    let gloss = sprintf "zobacz „%s%s” %s" sh (if sv = "" then "" else " " ^ sv) sl in
    fprintf file "%s\t%s\t%s\t%s\t%s\t%s\t\n"
        e.head e.pos e.head_variant s.variant "" gloss

let tab_of_entries filename l =
  File.file_out filename (fun file ->
    Xlist.iter l (fun e ->
      Xlist.iter e.senses (fun s ->
        print_entry_sense file e s)))

let _ =
  let sjp = load_sjp "data/SJP.xml" in
  let sjp = resolve_titles sjp in
  let sjp = assign_pos sjp in
  let sjp = List.sort sjp_compare (Xlist.rev_map sjp clean_titles) in
  tab_of_entries "results/SJP.tab" sjp;
  ()
