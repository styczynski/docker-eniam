open Xstd

let strony_listy = [
  "A", 49;
  "B", 51;
  "C", 49;
  "Ć", 1;
  "D", 60;
  "E", 25;
  "F", 30;
  "G", 38;
  "H", 23;
  "I", 20;
  "J", 17;
  "K", 96;
  "L", 27;
  "Ł", 7;
  "M", 60;
  "N", 69;
  "O", 73;
  "Ó", 1;
  "P", 199;
  "R", 62;
  "S", 111;
  "Ś", 10;
  "T", 46;
  "U", 31;
  "V", 2;
  "W", 87;
  "X", 1;
  "Y", 1;
  "Z", 78;
  "Ż", 6;
]

let list_address_genenerator l =
  Xlist.iter l (fun (letter,count) ->
    Int.iter 1 count (fun i ->
      let s = if i = 1 then "" else ";" ^ string_of_int i in
      Printf.printf "http://sjp.pwn.pl/sjp/lista/%s%s.html\n" letter s))

(* generowanie adresy_lista.tab *)
(* let _ =
  list_address_genenerator strony_listy;
  () *)

let match_string = "                            <li><img src=\"/theme/Dictionary/img/dictionary-api-source/s.svg?"
let match_string2 = "                            <li><img src=\"/theme/Dictionary/img/dictionary-api-source/s.svg?1444379457\" class=\"alfa-image svg-image\" title=\"Słownik języka polskiego\" onerror=\"this.onerror=null; this.src=&#039;http://sjp.pwn.pl/theme/Dictionary/img/dictionary-api-source/s.png?1417608895&#039;\" alt=\"\" /> <a href=\""
let match_string3 = "                            <li><img src=\"/theme/Dictionary/img/dictionary-api-source/s.svg?1444379457\" class=\"alfa-image\" title=\"Słownik języka polskiego\" alt=\"\" /> <a href=\""

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

let check_cut_infix pat s msg =
  match Xstring.split_delim pat s with
    [a;b] -> a,b
  | _ -> failwith (msg ^ ": " ^ s)

let full_split pat s =
  Xlist.map (Str.full_split (Str.regexp pat) s) (function
        Str.Text s -> s
      | Str.Delim s -> s)


let extract_line s =
  let s = check_cut_prefix [match_string2; match_string3] s "extract_line 1: " in
    (* if Xstring.check_prefix match_string2 s then Xstring.cut_prefix match_string2 s else
    if Xstring.check_prefix match_string3 s then Xstring.cut_prefix match_string3 s
    else failwith ("extract_line 1: " ^ s) in *)
  let s = check_cut_sufix ["</a></li>"] s "extract_line 2" in
    (* if Xstring.check_sufix "</a></li>" s then  Xstring.cut_sufix "</a></li>" s
    else failwith ("extract_line 2: " ^ s) in *)
  check_cut_infix "\">" s "extract_line 3"
  (* match Xstring.split_delim "\">" s with
    [url;name] -> url,name
  | _ -> failwith ("extract_line 3: " ^ s) *)

let list_address_extractor dir =
  let l = List.sort compare (Array.to_list (Sys.readdir dir)) in
  Xlist.map l (fun name ->
    name,
    List.rev (Xlist.fold (File.load_lines (dir ^ name)) [] (fun l line ->
      if Xstring.check_prefix match_string line then (extract_line line) :: l else l)))

(* generowanie adresy_hasla.tab *)
(* let _ =
  let l = list_address_extractor "../../../NLP resources/SJP/sjp.pwn.pl/sjp/lista/" in
  (* Xlist.iter l (fun (n,l) -> Printf.printf "%s %d\n" n (Xlist.size l)); *)
  Xlist.iter l (fun (_,l) ->
    Xlist.iter l (fun (url,name) ->
      Printf.printf "%s\t%s\n" name url));
  () *)

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

let split_title s =
  match Xstring.split_delim "\xC2\xA0" s with
    [lemma] -> lemma,""
  | [lemma;"I"] -> lemma,"I"
  | [lemma;"II"] -> lemma,"II"
  | [lemma;"III"] -> lemma,"III"
  | [lemma;"IV"] -> lemma,"IV"
  | [lemma;"V"] -> lemma,"V"
  | [lemma;"VI"] -> lemma,"VI"
  | _ -> failwith ("split_title: '" ^ s ^ "'")

let split_myslnik s =
  match Xstring.split_delim "-" s with
    [lemma] -> lemma
  | _ -> print_endline s; s

let split_spacja s =
  match Xstring.split_delim " " s with
    [lemma] -> lemma
  | _ -> print_endline s; s

let split_comma s =
  match Xstring.split_delim ", " s with
    [lemma] -> s
  | _ -> (*print_endline s;*) "###" (* hasła o złożonych nazwach np. "szkatułkowa forma, kompozycja utworu, dzieła itp." *)

let standard_chars = StringSet.of_list [
  "a";"ą";"b";"c";"ć";"d";"e";"ę";"f";"g";"h";"i";"j";"k";"l";"ł";"m";"n";"ń";"o";"ó";"p";"r";
  "s";"ś";"t";"u";"w";"y";"z";"ź";"ż";(*"-";",";" ";*)"v";"é";"à";"x";"ü";"q";"ê";"è";"ö";"ü";"ñ";
  "A";"Ą";"B";"C";"Ć";"D";"E";"Ę";"F";"G";"H";"I";"J";"K";"L";"Ł";"M";"N";"Ń";"O";"Ó";"P";"R";
  "S";"Ś";"T";"U";"W";"Y";"Z";"Ź";"Ż";"X";"V";"";"";"";"";"";"";"";"";"";"";"";"";"";"";]

let check_nonstandart_chars lemma =
  Xlist.fold (Xunicode.utf8_chars_of_utf8_string lemma) false (fun b c ->
    if StringSet.mem standard_chars c then b else true)

(* UWAGA: w słowniku występują hasła złożone zawierające ", ", wyrażenia wielosłownie,
dopowiedzenia w nawiasach, wyrazy obce, skróty, prefiksy, sufiksy, wyrażenia wielotokenowe *)
let make_lemma_map l =
  Xlist.fold l StringMap.empty (fun map (name,url) ->
    let lemma,variant = split_haslo name in
    (* let lemma = split_comma lemma in *)
    (* let _ = split_spacja lemma in *)
    (* let _ = split_myslnik lemma in *)
    if lemma = "###" then map else (
    (* if check_nonstandart_chars lemma then print_endline name; *)
    StringMap.add_inc map lemma [variant,url] (fun l -> (variant,url) :: l)))

let load_lu_list filename =
  File.fold_tab filename StringMap.empty (fun map -> function
      [name;cat] -> StringMap.add_inc map name (StringSet.singleton cat) (fun set -> StringSet.add set cat)
    | l -> failwith ("load_lu_list: " ^ String.concat "\t" l))

let find_url_for_lu lemma_map lu_map =
  StringMap.iter lu_map (fun lu _ ->
    try
      Xlist.iter (StringMap.find lemma_map lu) (fun (_,url) -> print_endline url)
    with Not_found -> print_endline (lu ^ " NOT FOUND"))

(* generowanie wybrane_adresy_hasla.tab *)
(* let _ =
  let l = File.load_tab "data/adresy_hasla.tab" (function [name;url] -> name,url | _ -> failwith "load adresy_hasla") in
  let lemma_map = make_lemma_map l in
  let lu_map = load_lu_list "../base_LU.tab" in
  find_url_for_lu lemma_map lu_map;
  () *)

let head_string1 = "            <div class=\"base-entry-head\"><h1>"
let head_string2 = "</h1></div>"

let rec select_haslo2 name = function
    [] -> failwith ("select_haslo2: " ^ name)
  | "</article>" :: _ -> []
  | line :: l -> line :: (select_haslo2 name l)

let rec select_haslo name = function
    [] -> failwith ("select_haslo: " ^ name)
  | line :: l ->
       if Xstring.check_prefix head_string1 line then
         select_haslo2 name (line :: l)
       else select_haslo name l

let haslo_extractor dir =
  let l = Array.to_list (Sys.readdir dir) in
  Xlist.fold l [] (fun l name ->
    if name = "lista" then l else
    (name, select_haslo name (File.load_lines (dir ^ name))) :: l)

let rec select_lines = function
    [] -> []
  | "            <div class=\"base-entry-line\">&nbsp;" :: l -> select_lines l
  | "                            <div class=\"base-entry-hint\">Słownik języka polskiego</div>" :: l -> select_lines l
  | "                    </div>" :: l -> select_lines l
  | "            <div class=\"base-entry-body\">" :: l -> select_lines l
  | "            <article>" :: l -> select_lines l
  | "    <div class=\"ribbon\">" :: l -> select_lines l
  | "        <div class=\"ribbon-section\">" :: l -> select_lines l
  | "    <div class=\"ribbon-element type-187126\">" :: l -> select_lines l
  | "    " :: l -> select_lines l
  | "</div>" :: l -> select_lines l
  | "    </div>" :: l -> select_lines l
  | " </div>" :: l -> select_lines l
  | line :: l -> line :: (select_lines l)

type token =
    Head of string
  | Tytul of string
  | Variant of string
  | SenseText of string
  | Href of string * string
  | Zob of string * string
  | Der of string
  | Example of string
  | Kwal of string * string
  | Skrot of string
  | Wym of string * string * string * string * string
  | Sense of (string * string * string list) list (* variant * gloss * examples *)
  | Zob2 of string * string * string * string list
  | Word of string

let string_of_token = function
    Head s -> Printf.sprintf "Head(%s)" s
  | Tytul s -> Printf.sprintf "Tytul(%s)" s
  | Variant s -> Printf.sprintf "Variant(%s)" s
  | SenseText s -> Printf.sprintf "SenseText(%s)" s
  | Href(s,t) -> Printf.sprintf "Href(%s,%s)" s t
  | Zob(s,t) -> Printf.sprintf "Zob(%s,%s)" s t
  | Der s -> Printf.sprintf "Der(%s)" s
  | Example s -> Printf.sprintf "Example(%s)" s
  | Kwal(s,t) -> Printf.sprintf "Kwal(%s,%s)" s t
  | Skrot s -> Printf.sprintf "Skrot(%s)" s
  | Wym(s,t,a,b,c) -> Printf.sprintf "Wym(%s,%s,%s,%s,%s)" s t a b c
  | Sense l -> Printf.sprintf "Sense"
  | Zob2(s,t,r,l) -> Printf.sprintf "Zob(%s,%s,%s,[%s])" s t r (String.concat ";" l)
  | Word s -> Printf.sprintf "Word(%s)" s

let mark_words = function
    Example "a" -> Word "a"
  | Example "a, b, c..." -> Word "a, b, c..."
  | Example "a zatem" -> Word "a zatem"
  | Example "bez względu na to skąd" -> Word "bez względu na to skąd"
  | Example "buszel" -> Word "buszel"
  | Example "bym, byś, byśmy, byście" -> Word "bym, byś, byśmy, byście"
  | Example "c" -> Word "c"
  | Example "ch" -> Word "ch"
  | Example "chyba że" -> Word "chyba że"
  | Example "chyba żeby" -> Word "chyba żeby"
  | Example "e" -> Word "e"
  | Example "gdzie" -> Word "gdzie"
  | Example "gdzie indziej" -> Word "gdzie indziej"
  | Example "hektometr" -> Word "hektometr"
  | Example "hektar" -> Word "hektar"
  | Example "i" -> Word "i"
  | Example "igrek" -> Word "igrek"
  | Example "ile" -> Word "ile"
  | Example "ile razy" -> Word "ile razy"
  | Example "im" -> Word "im"
  | Example "ipsylon" -> Word "ipsylon"
  | Example "j" -> Word "j"
  | Example "jak i" -> Word "jak i"
  | Example "jak również" -> Word "jak również"
  | Example "jak też" -> Word "jak też"
  | Example "kędy" -> Word "kędy"
  | Example "kh" -> Word "kh"
  | Example "kiedy" -> Word "kiedy"
  | Example "kiedy indziej" -> Word "kiedy indziej"
  | Example "ks" -> Word "ks"
  | Example "kto" -> Word "kto"
  | Example "który" -> Word "który"
  | Example "m" -> Word "m"
  | Example "M" -> Word "M"
  | Example "najpierw" -> Word "najpierw"
  | Example "nie" -> Word "nie"
  | Example "o" -> Word "o"
  | Example "o ile" -> Word "o ile"
  | Example "o tyle" -> Word "o tyle"
  | Example "skąd" -> Word "skąd"
  | Example "tak" -> Word "tak"
  | Example "ten" -> Word "ten"
  | Example "to" -> Word "to"
  | Example "tu" -> Word "tu"
  | Example "ty" -> Word "ty"
  | Example "tyle" -> Word "tyle"
  | Example "tym" -> Word "tym"
  | Example "u" -> Word "u"
  | Example "w" -> Word "w"
  | Example "x" -> Word "x"
  | Example "X" -> Word "X"
  | Example "y" -> Word "y"
  | Example "z" -> Word "z"
  | Example "zarówno..., jak" -> Word "zarówno..., jak"
  | Example "z jakiegokolwiek miejsca" -> Word "z jakiegokolwiek miejsca"
  | Example "μ" -> Word "μ"
  | Example "π" -> Word "π"
  | Example "Π, π" -> Word "Π, π"
  | Example "Φ, ϕ" -> Word "Φ, ϕ"
  | Example "praktycznie biorąc" -> Word "praktycznie biorąc"
  | Example "praktycznie rzecz biorąc" -> Word "praktycznie rzecz biorąc"
  | Example "co prawda..., ale (jednak, lecz)..." -> Word "co prawda..., ale (jednak, lecz)..."
  | Example "wprawdzie..., ale (jednak, lecz)..." -> Word "wprawdzie..., ale (jednak, lecz)..."
  | Example "zarówno ..., jak (i, też) ..." -> Word "zarówno ..., jak (i, też) ..."
  | Example "tylekroć, tyle razy, zawsze" -> Word "tylekroć, tyle razy, zawsze"
  | Example s -> (*if Xstring.size s < 30 then print_endline s;*) Example s
  | t -> t


let rec merge_sense_text rev = function
    SenseText s :: SenseText t :: l -> merge_sense_text rev (SenseText(s^t) :: l)
  | SenseText s :: Skrot t :: l -> merge_sense_text rev (SenseText(s^t) :: l)
  | SenseText s :: Word t :: l -> merge_sense_text rev (SenseText(s^"„"^t^"”") :: l)
  | SenseText s :: l -> merge_sense_text (SenseText s :: rev) l
  | Example s :: l -> merge_sense_text (Example s :: rev) l
  | [] -> List.rev rev
  | l -> failwith ("merge_sense_text: " ^ String.concat " " (Xlist.map l string_of_token))

let rec clean_sense_text rev = function
    SenseText "." :: l -> clean_sense_text rev l
  | SenseText " " :: l -> clean_sense_text rev l
  | SenseText ", " :: l -> clean_sense_text rev l
  | SenseText "; " :: l -> clean_sense_text rev l
  | SenseText s :: l ->
      let s = if Xstring.check_prefix ", " s then Xstring.cut_prefix ", " s else s in
      let s = if Xstring.check_sufix ", np. " s then Xstring.cut_sufix ", np. " s else s in
      let s = if Xstring.check_sufix ", np." s then Xstring.cut_sufix ", np." s else s in
      let s = if Xstring.check_sufix ". np. " s then Xstring.cut_sufix ". np. " s else s in
      let s = if Xstring.check_sufix ", np.: " s then Xstring.cut_sufix ", np.: " s else s in
      let s = if Xstring.check_sufix " np." s then Xstring.cut_sufix " np." s else s in
      clean_sense_text ((SenseText s) :: rev) l
  | t :: l -> clean_sense_text (t :: rev) l
  | [] -> List.rev rev

let rec split_sense_text rev = function
    SenseText s :: Example t1 :: Example t2 :: Example t3 :: l -> split_sense_text (("",s,[t1;t2;t3]) :: rev) l
  | SenseText s :: Example t1 :: Example t2 :: l -> split_sense_text (("",s,[t1;t2]) :: rev) l
  | SenseText s :: Example t1 :: l -> split_sense_text (("",s,[t1]) :: rev) l
  | SenseText s :: l -> split_sense_text (("",s,[]) :: rev) l
  | [] -> List.rev rev
  | l -> failwith ("split_sense_text: " ^ String.concat " " (Xlist.map l string_of_token))

let process_sense l =
  let l = Xlist.map l mark_words in
  let l = merge_sense_text [] l in
  let l = clean_sense_text [] l in
  let l = split_sense_text [] l in
  Sense l

let rec parse_line = function
    ["            "; "<"; "div class=\"base-entry-head\""; ">"; "<"; "h1"; ">"; head; "<"; "/h1"; ">"; "<"; "/div"; ">"] -> [Head head]
  | "<" :: "div class=\"znacz\"" :: ">" :: line -> parse_line line
  | "<" :: "/div" :: ">" :: line -> parse_line line
  | "<" :: "br" :: ">" :: line -> parse_line line
  | ", " :: line -> parse_line line
  | "; " :: line -> parse_line line
  | ". " :: line -> parse_line line
  | "." :: line -> parse_line line
  | "• " :: line -> parse_line line
  | " • " :: line -> parse_line line
  | "<" :: "span class=\"tytul\"" :: ">" :: tytul :: "<" :: "/span" :: ">" :: line -> Tytul tytul :: (parse_line line)
  | " " :: line -> parse_line line
  | "<" :: "b" :: ">" :: variant :: "<" :: "/b" :: ">" :: "<" :: "b" :: ">" :: name :: "<" :: "/b" :: ">" :: line -> Variant variant :: Href("",name) :: (parse_line line)
  | "<" :: "b" :: ">" :: variant :: "<" :: "/b" :: ">" :: line -> Variant variant :: (parse_line line)
  | "«" :: line -> parse_sense [] line
  | "<" :: "i" :: ">" :: "zob." :: "<" :: "/i" :: ">" :: " " :: "<" :: href :: ">" :: name :: "<" :: "/a" :: ">" :: " w zn. 7, 8. " :: line -> Zob(href,name ^ " w zn. 7, 8. ") :: (parse_line line)
  | "<" :: "i" :: ">" :: "zob." :: "<" :: "/i" :: ">" :: " " :: "<" :: href :: ">" :: name :: "<" :: "/a" :: ">" :: line -> Zob(href,name) :: (parse_line line)
  | "<" :: "i" :: ">" :: "zob." :: "<" :: "/i" :: ">" :: name :: "<" :: "/div" :: ">" :: line -> Zob("",name) :: (parse_line line)
  | "<" :: "i" :: ">" :: "zob." :: "<" :: "/i" :: ">" :: " " :: "<" :: href :: ">" :: name :: "<" :: "span class=\"skrot-inny\"" :: ">" :: name2 :: "<" :: "/span" :: ">" :: name3 :: "<" :: "/a" :: ">" :: line -> Zob(href,name ^ name2 ^ name3) :: (parse_line line)
  | "<" :: "i" :: ">" :: "zob." :: "<" :: "/i" :: ">" :: " " :: "<" :: href :: ">" :: name :: "<" :: "span class=\"skrot-inny\"" :: ">" :: "<" :: "abbr title=\"znaczenie, znaczy\"" :: ">" :: name2 :: "<" :: "/abbr" :: ">" :: "<" :: "/span" :: ">" :: name3 :: "<" :: "/a" :: ">" :: line -> Zob(href,name ^ name2 ^ name3) :: (parse_line line)
  | "stopień najwyższy od wysoko." :: line -> process_sense [SenseText "stopień najwyższy od ";Word "wysoko"] :: (parse_line line)
  | "stopień wyższy od " :: "<" :: href :: ">" :: "dużo" :: "<" :: "/a" :: ">" :: " lub od " :: "<" :: href2 :: ">" :: "wiele" :: "<" :: "/a" :: ">" :: ", " ::
        "<" :: "span class=\"skrot-inny\"" :: ">" :: "np." :: "<" :: "/span" :: ">" :: " " :: "<" :: "i" :: ">" :: example :: "<" :: "/i" :: ">"  :: line ->
           process_sense [SenseText "stopień wyższy od ";Word "dużo";SenseText " lub od ";Word "wiele";SenseText " np.";Example example] :: (parse_line line)
  | " stopień najwyższy od " :: "<" :: "i" :: ">" :: "mało" :: "<" :: "/i" :: ">" :: line -> process_sense [SenseText "stopień najwyższy od ";Word "mało"] :: (parse_line line)
  | " stopień wyższy od " :: "<" :: href :: ">" :: "mało" :: "<" :: "/a" :: ">" :: line -> process_sense [SenseText "stopień wyższy od ";Word "mało"] :: (parse_line line)
  | "<" :: "span class=\"tytul-der\"" :: ">" :: der :: "<" :: "/span" :: ">" :: line -> Der der :: (parse_line line)
  | "<" :: "span class=\"kwal\"" :: ">" :: "<" :: title :: ">" :: text :: "<" :: "/abbr" :: ">" :: "<" :: "/span" :: ">" :: line -> Kwal(title,text) :: (parse_line line)
  | "<" :: "span class=\"kwal\"" :: ">" :: text :: "<" :: "/span" :: ">" :: line -> Kwal("",text) :: (parse_line line)
  | "[" :: "<" :: "i" :: ">" :: "wym." :: "<" :: "/i" :: ">" :: wym :: "]" :: line -> Wym(wym,"","","","") :: (parse_line line)
  | "[" :: "<" :: "i" :: ">" :: "wym." :: "<" :: "/i" :: ">" :: wym :: "<" :: "span class=\"akcent\"" :: ">" :: akcent :: "<" :: "/span" :: ">" :: "]" :: line -> Wym(wym,akcent,"","","") :: (parse_line line)
  | "[" :: "<" :: "i" :: ">" :: "wym." :: "<" :: "/i" :: ">" :: wym :: "<" :: "span class=\"akcent\"" :: ">" :: akcent :: "<" :: "/span" :: ">" :: wym2 :: "]" :: line -> Wym(wym,akcent,wym2,"","") :: (parse_line line)
  | "[" :: "<" :: "i" :: ">" :: "wym." :: "<" :: "/i" :: ">" :: wym :: "<" :: "span class=\"akcent\"" :: ">" :: akcent :: "<" :: "/span" :: ">" :: wym2 :: "<" :: "span class=\"akcent\"" :: ">" :: akcent2 :: "<" :: "/span" :: ">" :: wym3 :: "]" :: line -> Wym(wym,akcent,wym2,akcent2,wym3) :: (parse_line line)
  | [] -> []
  | l -> Printf.printf "\"%s\"\n" (String.concat "\" :: \"" l); []

and parse_sense rev = function
    "<" :: "i" :: ">" :: example :: "<" :: "/i" :: ">" :: line -> parse_sense (Example example :: rev) line
  | "<" :: "span class=\"skrot-inny\"" :: ">" :: abbr :: "<" :: "/span" :: ">" :: line -> parse_sense (Skrot abbr :: rev) line
  | "<" :: "span class=\"skrot-inny\"" :: ">" :: "<" :: "i" :: ">" :: s :: "<" :: "/i" :: ">" :: "<" :: "/span" :: ">" :: line -> parse_sense (SenseText("„"^s^"”") :: rev) line
  | "<" :: "span class=\"gram\"" :: ">" :: s :: "<" :: "/span" :: ">" :: line -> parse_sense (SenseText s :: rev) line
  | "<" :: "sup" :: ">" :: s :: "<" :: "/sup" :: ">" :: line -> parse_sense (SenseText("<sup>"^s^"</sup>") :: rev) line
  | "<" :: "sub" :: ">" :: s :: "<" :: "/sub" :: ">" :: line -> parse_sense (SenseText("<sub>"^s^"</sub>") :: rev) line
  | "»" :: line -> (process_sense (List.rev rev)) :: (parse_line line)
  | "<" :: l -> Printf.printf "XXX \"%s\"\n" (String.concat "\" :: \"" ("<" :: l)); []
  | "«" :: _ -> failwith "parse_sense"
  | ">" :: _ -> failwith "parse_sense"
  | []-> failwith "parse_sense"
  | s :: line -> parse_sense (SenseText s :: rev) line

let get_head = function
    Head h :: l -> h, l
  | l -> failwith ("get_head: " ^ String.concat " :: " (Xlist.map l string_of_token))

let rec get_titles titles wym = function
    Tytul t :: l -> get_titles (t :: titles) wym l
  | Wym(s,t,a,b,c) :: l -> get_titles titles ((s,t,a,b,c) :: wym) l
  | l -> List.rev titles, List.rev wym, l

let rec get_ders = function
    Der s :: l -> s :: get_ders l
  | []-> []
  | l -> failwith "get_ders"


let rec split_variants found rev = function
    Variant s :: l ->
      let found = if rev = [] then found else (List.rev rev :: found) in
      split_variants found [Variant s] l
  | Der s :: l ->
      let found = if rev = [] then found else (List.rev rev :: found) in
      List.rev found, get_ders (Der s :: l)
  | t :: l -> split_variants found (t :: rev) l
  | [] ->
      let found = if rev = [] then found else (List.rev rev :: found) in
      List.rev found, []

let variant_names = StringSet.of_list ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "10"; "11"; "12"; "13"; ""; ]
let kwal_names_a = StringSet.of_list [""]
let kwal_names_b = StringSet.of_list ["daw."; "pot."; ""; ""; ""; ""; ""; ""; ]

let process_zob s name =
  let id,head,head_variant =
    if s = "" then "","","" else
    let s = check_cut_prefix ["a href=\"http://sjp.pwn.pl/sjp/"] s "process_zob 1" in
    let s = check_cut_sufix ["\" class=\"anchor\""] s "process_zob 2" in
    let id,s = check_cut_infix ".html\" title=\"" s "process_zob 3" in
    let head,pwn_id = check_cut_infix "\" pwn-id=\"" s "process_zob 4" in
    let _,pwn_id2 = check_cut_infix ";" id "process_zob 5" in
    if pwn_id <> pwn_id2 then failwith "process_zob 6" else
    let head,head_variant = split_haslo head in
    id,head,head_variant in
  (* Printf.printf "%s %s %s\n" id head head_variant; *)
  let l = Str.split (Str.regexp " \\|\\.\\|,") name in
  let head_variant2,l = match l with
      "I" :: l -> "I",l
    | "II" :: l -> "II",l
    | "III" :: l -> "III",l
    | l -> "",l in
  let head2,l = match l with
      [s] -> s,[]
    | [s;t] -> s ^ " " ^ t,[]
    | s :: "w" :: "zn" :: l -> s,l
    | l -> "",l in
  let variants = List.flatten (Xlist.map l (function
      "" -> []
    | "1" -> ["1"]
    | "2" -> ["2"]
    | "3" -> ["3"]
    | "4" -> ["4"]
    | "7" -> ["7"]
    | "8" -> ["8"]
    | "1–4" -> ["1";"2";"3";"4"]
    | s -> failwith ("process_zob 7: " ^ s))) in
  if head <> "" && head <> "związek koordynacyjny, kompleksowy" && head <> head2 then failwith ("process_zob 8: " ^ head ^ " " ^ head2) else
  if head_variant <> head_variant2 then failwith "process_zob 9" else
  Zob2(id,head2,head_variant2,variants)

let add_word s = function
    (v,gloss,ex) :: l -> (v,"„" ^ s ^ "” " ^ gloss, ex) :: l
  | [] -> failwith "add_word"

let pos_list = [
  "liczebnik", "num";
  "partykuła", "qub";
  "przyimek", "prep";
  "przysłówek", "adv";
  "spójnik", "conj";
  "wykrzyknik", "interj";
  "zaimek", "pron";
  ]

let find_pos senses =
  let set = Xlist.fold senses StringSet.empty (fun set (_,_,s) ->
    match s with
      Zob2 _ -> set
    | Sense((_,gloss,_) :: _) ->
          Xlist.fold pos_list set (fun set (pat,pos) ->
            if Xstring.check_prefix pat gloss then StringSet.add set pos else set)
    | _ -> failwith "find_pos 1") in
  match StringSet.to_list set with
    [] -> ""
  | [pos] -> pos
  | l -> String.concat "_" l

let rec add_subsense_variants_rec i = function
    [] -> []
  | (v,gloss,ex) :: l ->
       let v = String.make 1 (Char.chr (Char.code 'a' + i)) in
       (v,gloss,ex) :: (add_subsense_variants_rec (i+1) l)

let add_subsense_variants = function
    Sense[v,gloss,ex] -> Sense[v,gloss,ex]
  | Sense l -> Sense(add_subsense_variants_rec 0 l)
  | t -> t

let rec process_haslo name l =
  let l = List.flatten (Xlist.map (select_lines l) (fun s ->
    parse_line (full_split "<\\|>\\|«\\|»\\|\\]\\|\\[" s))) in
  let head,l = get_head l in
  let titles,wym,l = get_titles [] [] l in
  let ll,der = split_variants [] [] l in
  let senses = Xlist.map ll (function
    | [Sense s] -> "","","",Sense s
    | [Zob(s,t)] -> "","","",process_zob s t
    | [Kwal(a,b); Sense s] -> "",a,b,Sense s
    | [Kwal(a,b); Zob(s,t)] -> "",a,b,process_zob s t
    | [Variant v; Sense s] -> v,"","",Sense s
    | [Variant v; Zob(s,t)] -> v,"","",process_zob s t
    | [Variant v; Href(s,t); Sense st] -> v,"","",Sense(add_word t st)
    | [Variant v; Kwal(a,b); Sense s] -> v,a,b,Sense s
    | [Variant v; Href(s,t); Kwal(a,b); Sense st] -> v,a,b,Sense(add_word t st)
    | [Variant v; Kwal(a,b); Zob(s,t)] -> v,a,b,process_zob s t
    | l -> failwith (String.concat " :: " (Xlist.map l string_of_token))) in
  let id = check_cut_sufix [".html"] name "process_haslo 1" in
      (* if Xstring.check_sufix ".html" name then Xstring.cut_sufix ".html" name
      else failwith "process_haslo" in *)
  let head,head_variant = split_haslo head in
  (* if head_variant <> "" then print_endline head_variant; *)
  (* if check_nonstandart_chars head then print_endline head; *)
  let titles = Xlist.map titles split_title in
  (* Xlist.iter titles (fun (title,title_variant) ->
    if check_nonstandart_chars title then print_endline title);  *)
  (* Xlist.iter der print_endline; *)
  let senses = Xlist.map senses (fun (v,a,qual,s) ->
    let v =
      if v = "" then "" else
      check_cut_sufix [". "] v "process_haslo 2" in
      (* if Xstring.check_sufix ". " v then Xstring.cut_sufix ". " v
      else failwith "process_haslo" in *)
    let s = add_subsense_variants s in
    v,(*a,*)qual,s) in
  (* Xlist.iter senses (fun (v,(*a,*)b,s) ->
    print_endline (string_of_token s);
    (* if not (StringSet.mem variant_names v) then print_endline v; *)
    (* if a = "" && b <> "" then print_endline b; *)
    (*if not (StringSet.mem kwal_names_a a) then print_endline a;
    if not (StringSet.mem kwal_names_b b) then print_endline b*)); *)
  let pos = find_pos senses in
  id,head,head_variant,pos,titles,wym,senses,der

let xml_of_sense = function
    Zob2(id,head,head_variant,variants) ->
      [Xml.Element("see",["id",id;"head",head] @ (if head_variant = "" then [] else ["head_variant",head_variant]),
        Xlist.map variants (fun v -> Xml.Element("sense",["variant",v],[])))]
  | Sense l ->
      Xlist.map l (fun (v,gloss,exs) ->
        Xml.Element("subsense",(if v = "" then [] else ["variant",v]),Xml.PCData gloss :: Xlist.map exs (fun ex -> Xml.Element("example",[],[Xml.PCData ex]))))
  | _ -> failwith "xml_of_sense"

let xml_of_hasla l =
  Xml.Element("sjp",[],Xlist.rev_map l (fun (id,head,head_variant,pos,titles,wym,senses,der) ->
    Xml.Element("entry",["id",id;"head",head] @ (if head_variant = "" then [] else ["head_variant",head_variant]) @ (if pos = "" then [] else ["pos",pos]),
      Xlist.map titles (fun (title,title_variant) ->
        Xml.Element("title",(if title_variant = "" then [] else ["title_variant",title_variant]),[Xml.PCData title])) @
      Xlist.map wym (fun (a,b,c,d,e) ->
        Xml.Element("pron",[],[Xml.PCData a] @
          (if b = "" then [] else [Xml.Element("akcent",[],[Xml.PCData b])]) @
          (if c = "" then [] else [Xml.PCData c]) @
          (if d = "" then [] else [Xml.Element("akcent",[],[Xml.PCData d])]) @
          (if e = "" then [] else [Xml.PCData e]))) @
      Xlist.map senses (fun (v,qual,s) ->
        Xml.Element("sense",
          (if v = "" then [] else ["variant",v]) @
          (if qual = "" then [] else ["qualifier",qual]),
          xml_of_sense s)) @
      Xlist.map der (fun d -> Xml.Element("der",[],[Xml.PCData d])))))

let _ =
  let hasla = haslo_extractor "../../../NLP resources/SJP/sjp.pwn.pl/sjp/" in
  let hasla = Xlist.rev_map hasla (fun (name,l) ->
    (* print_endline name; *)
    process_haslo name l) in
  print_endline (Xml.to_string_fmt (xml_of_hasla hasla));
  ()
