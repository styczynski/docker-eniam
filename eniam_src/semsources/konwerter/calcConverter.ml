
open Xstd
open Printf

let semroles_filename = "data/Anotacja jednostek leksykalnych - ENIAM.html"
let interj_filename = "data/interj.html"

type field = {top: bool; bottom: bool; value: string}

let empty_field = {top=false; bottom=false; value=""}

type subentry = {
  thematic_role: string list;
  primary_selprefs: string list;
  dependent_selprefs: string list;
  examples: string list;
  examples_sources: string list}

type entry = {lexeme: string; pos: string; prep_sem: string; comments: string list;
  primary_pos: string list; dependent_pos: string list;
  dependent_case: string list; coordination: string list;
  subentries: subentry list;
  sjp_id1: string; sjp_id2: string; sjp_id3: string;
  gloss: string; sem_phenomenon: string list;
  phenomenon_details: string list;
  comp_type: string list; sentence_type: string list;
  etymology: string; mwe: string}

let empty_subentry = {
  thematic_role=[];
  primary_selprefs=[];
  dependent_selprefs=[];
  examples=[];
  examples_sources=[]}

let empty_entry = {lexeme=""; pos=""; prep_sem=""; comments=[];
  primary_pos=[]; dependent_pos=[];
  dependent_case=[]; coordination=[];
  subentries=[];
  sjp_id1=""; sjp_id2=""; sjp_id3="";
  gloss=""; sem_phenomenon=[]; phenomenon_details=[];
  comp_type=[]; sentence_type=[];
  etymology=""; mwe=""}


let process_a = function
    Xml.Element("A",_,[
      Xml.Element("h1",_,[
        Xml.PCData _;
        Xml.Element("em",[],[
          Xml.PCData s])])]) (*as xml*) -> (*print_endline (Xml.to_string_fmt xml);*) s
  | xml -> failwith ("process_a: " ^ Xml.to_string xml)

let process_column a = function
    [Xml.PCData t] -> {a with value = t}
  | [Xml.Element("b",_,[Xml.PCData t])] -> {a with value = t}
  | [Xml.Element("b",_,[Xml.Element("font",_,[Xml.PCData t])])] -> {a with value = t}
  | [Xml.Element("b",_,[Xml.Element("font",_,[Xml.Element("br",[],[])])])] -> a
  | [Xml.Element("b",_,[Xml.Element("br",[],[])])] -> a
  | [Xml.Element("b",_,[Xml.Element("i",_,[Xml.Element("font",_,[Xml.PCData t])])])] -> {a with value = t}
  | [Xml.Element("b",_,[Xml.Element("i",_,[Xml.Element("font",_,[Xml.Element("br",[],[])])])])] -> a
  | [Xml.Element("i",_,[Xml.Element("font",_,[Xml.PCData t])])] -> {a with value = t}
  | [Xml.Element("i",_,[Xml.Element("font",_,[Xml.Element("br",[],[])])])] -> a
  | [Xml.Element("font",_,[Xml.PCData t])] -> {a with value = t}
  | [Xml.Element("font",_,[Xml.Element("br",[],[])])] -> a
  | [Xml.Element("font",_,[Xml.PCData t;Xml.Element("br",[],[])])] -> {a with value = t}
  | [Xml.Element("br",[],[])] -> a
  | Xml.Element("a",_,[]) :: _ -> a
  | [] -> failwith "process_column: empty"
  | xml :: l -> failwith ("process_column: " ^ Xml.to_string xml)

let process_style a = function
    "border-bottom: 1px solid #000000" -> {a with bottom=true}
  | "border-bottom: 3px double #000000" -> {a with bottom=true}
  | "border-bottom: 1px solid #757070" -> {a with bottom=true}
  | "border-bottom: 1px dotted #000000" -> {a with bottom=true}
  | "border-bottom: 1px dotted #757070" -> {a with bottom=true}
  | "border-right: 1px dotted #757070" -> a
  | "border-top: 1px solid #000000" -> {a with top=true}
  | "border-top: 3px double #000000" -> {a with top=true}
  | "border-top: 1px solid #757070" -> {a with top=true}
  | "border-top: 1px dotted #000000" -> {a with top=true}
  | s -> failwith ("process_style: " ^ s)

let rec process_column_attrs a = function
    ("align",_) :: l ->  process_column_attrs a l
  | ("valign",_) :: l ->  process_column_attrs a l
  | ("height",_) :: l ->  process_column_attrs a l
  | ("style",s) :: l -> process_column_attrs (Xlist.fold (Xstring.split "; " s) a process_style) l
  | ("sdval",_) :: l ->  process_column_attrs a l
  | ("sdnum",_) :: l ->  process_column_attrs a l
  | ("bgcolor",_) :: l ->  process_column_attrs a l
  | [] -> a
  | (a,_) :: l -> failwith ("process_column_attrs: " ^ a)

let rec process_columns rev = function
    Xml.Element("td",attrs,c) :: l ->
      let a = process_column_attrs empty_field attrs in
      let a = process_column a c in
      process_columns (a :: rev) l
  | [] -> List.rev rev
  | xml :: _ -> failwith ("process_columns: " ^ Xml.to_string xml)

let rec process_rows rev = function
    Xml.Element("colgroup",_,_) :: l -> process_rows rev l
  | Xml.Element("tr",_,columns) :: l -> process_rows (process_columns [] columns :: rev) l
  | [] -> List.rev rev
  | xml :: _ -> failwith ("process_rows: " ^ Xml.to_string xml)

let process_table = function
    Xml.Element("table",_,rows) -> process_rows [] rows
  | xml -> failwith ("process_table: " ^ Xml.to_string xml)

let rec process_body = function
    (Xml.Element("A",_,_) as a) :: (Xml.Element("table",_,_) as t) :: l ->
      let name = process_a a in
      let tab = process_table t in
      (name,tab) :: (process_body l)
  | (Xml.Element("table",_,_) as t) :: l ->
      let tab = process_table t in
      ("",tab) :: (process_body l)
  | (Xml.Element("hr",_,_) (*as xml*)) :: l -> (*print_endline (Xml.to_string_fmt xml);*) process_body l
  | (Xml.Element("p",_,_) (*as xml*)) :: l -> (*print_endline (Xml.to_string_fmt xml);*) process_body l
  | [] -> []
  | xml :: _ -> failwith ("process_body: " ^ Xml.to_string xml)


let load_data filename =
  let xml = try Xml.parse_file filename with Xml.Error e -> print_endline (Xml.error e); failwith "load_data" in
  match xml with
    Xml.Element("html",[],[
      Xml.Element("head",_,_);
      Xml.Element("body",_,l)]) -> process_body l
  | _ -> failwith "load_data 2"

type line = Sep | Line of field list

let rec split_into_entries_rec found rev = function
    [] -> if rev = [] then List.rev found else List.rev ((List.rev rev) :: found)
  | Sep :: l ->
      if rev = [] then split_into_entries_rec found rev l else
      split_into_entries_rec ((List.rev rev) :: found) [] l
  | Line line :: l -> split_into_entries_rec found (line :: rev) l

let split_into_entries tab =
  let tab = List.flatten (List.rev (Xlist.rev_map tab (function
      a :: line ->
        (* if is_empty_line (a :: line) then [Sep] else *)
        (if a.top then [Sep] else []) @ [Line(a :: line)] @ (if a.bottom then [Sep] else [])
    | _ -> failwith "split_into_entries"))) in
  split_into_entries_rec [] [] tab

let split_into_entries_additional (names,tab) =
  if names = "Wyrażenia przyimkowe" || names = "Spójniki współrzędne" then
    let l = List.hd (List.rev tab) in
    names,(List.rev (List.tl (List.rev tab))) @ (List.rev (Xlist.rev_map l (fun line -> [line])))
  else names,tab

let merge_subentries (e,l) =
  e, split_into_entries l

let split_subentries (e,l) =
  let ll = Xlist.map l split_into_entries in
  let subentries = List.rev (Xlist.fold2 e.subentries ll [] (fun subentries subentry l ->
    Xlist.map l (fun _ -> subentry) :: subentries)) in
  {e with subentries=List.flatten subentries}, List.flatten ll


let is_empty_line l =
  Xlist.fold l true (fun b a -> if a.value = "" then b else false)

let is_empty_entry l =
  Xlist.fold l true (fun b a -> if is_empty_line a then b else false)

let remove_empty_entries tab =
  List.rev (Xlist.fold tab [] (fun tab e ->
    if is_empty_entry e then tab else e :: tab))


let extact_headings = function
    [headings] :: tab -> Xlist.map headings (fun a -> a.value), tab
  | _ -> failwith "extact_headings"

let remove_empty_column names (name,headings,entries) =
  if Xlist.mem names (List.hd headings) then
    name,List.tl headings,List.rev (Xlist.rev_map entries (fun (e,entry) ->
      e,Xlist.map entry (function
        a :: l -> if a.value = "" then l else failwith ("remove_empty_column: " ^ a.value)
      | [] -> failwith "remove_empty_column")))
  else name,headings,entries

let print_entry e =
  let comments = String.concat "\n" e.comments in
  let comments = if comments = "" then comments else "\n" ^ comments in
  Printf.printf "%s\t%s%s\n" e.lexeme e.prep_sem comments

let is_empty = function
    "" -> true
  | "--" -> true
  | "---" -> true
  | _ -> false

let extract_single_value names set_value_fun (name,headings,entries) =
  if Xlist.mem names (List.hd headings) then
    name, List.tl headings, List.rev (Xlist.rev_map entries (fun (e,entry) ->
      let l,entry = Xlist.fold entry ([],[]) (fun (l,entry) -> function
          a :: line -> if is_empty a.value then l,line :: entry else a.value :: l,line :: entry
        | [] -> failwith "extract_single_value 1") in
      let s = match l with
          [] -> ""
        | [s] -> (*print_endline s;*) s
        | _ -> failwith ("extract_single_value 2: " ^ e.lexeme) in
      set_value_fun e s,List.rev entry))
  else name,headings,entries

let extract_single_subentry_value names set_value_fun (name,headings,entries) =
  if Xlist.mem names (List.hd headings) then
    name, List.tl headings, List.rev (Xlist.rev_map entries (fun (e,entry) ->
      let ll,entry = Xlist.fold entry ([],[]) (fun (ll,entry) subentry ->
        let l,subentry = Xlist.fold subentry ([],[]) (fun (l,subentry) -> function
            a :: line -> if is_empty a.value then l,line :: subentry else a.value :: l,line :: subentry
          | [] -> failwith "extract_single_subentry_value 1") in
        (List.rev l) :: ll, (List.rev subentry) :: entry) in
      let s = match List.flatten ll with
          [] -> ""
        | [s] -> (*print_endline s;*) s
        | l -> print_endline ("extract_single_subentry_value 2: " ^ e.lexeme ^ " " ^ String.concat ";" l); String.concat ";" l in
      set_value_fun e s,List.rev entry))
  else name,headings,entries

let extract_multiple_value names set_value_fun (name,headings,entries) =
  if Xlist.mem names (List.hd headings) then
    name, List.tl headings, List.rev (Xlist.rev_map entries (fun (e,entry) ->
      let l,entry = Xlist.fold entry ([],[]) (fun (l,entry) -> function
          a :: line -> if is_empty a.value then l,line :: entry else a.value :: l,line :: entry
        | [] -> failwith "extract_single_value 1") in
      set_value_fun e (List.rev l),List.rev entry))
  else name,headings,entries

let add_subentry_value set_value_fun e ll =
  if e.subentries = [] then
    {e with subentries=Xlist.map ll (set_value_fun empty_subentry)}
  else {e with subentries=Xlist.map2 e.subentries ll set_value_fun}

let extract_subentry_value names set_value_fun (name,headings,entries) =
  if Xlist.mem names (List.hd headings) then
    name, List.tl headings, List.rev (Xlist.rev_map entries (fun (e,entry) ->
      let ll,entry = Xlist.fold entry ([],[]) (fun (ll,entry) subentry ->
        let l,subentry = Xlist.fold subentry ([],[]) (fun (l,subentry) -> function
            a :: line -> if is_empty a.value then l,line :: subentry else a.value :: l,line :: subentry
          | [] -> failwith "extract_subentry_value 1") in
        (List.rev l) :: ll, (List.rev subentry) :: entry) in
      add_subentry_value set_value_fun e (List.rev ll),List.rev entry))
  else name,headings,entries

let thematic_role_translation = Xlist.fold [
  "Conditon",["Condition"];
  "Time ",["Time"];
  "Attribute, Duration, Manner",["Attribute";"Duration";"Manner"];
  "Attribute, Manner",["Attribute";"Manner"];
  "Attribute, Manner, Measure",["Attribute";"Manner";"Measure"];
  "Location, Location Goal, Time, Tme Goal",["Location";"Location Goal";"Time";"Time Goal"];
  "Manner'",["Manner"];
  "Measure, Duration, Time, Attribute",["Measure";"Duration";"Time";"Attribute"];
  "Time, Duration, Attribute, Manner",["Time";"Duration";"Attribute";"Manner"];
  "Time, Location",["Time";"Location"];
  "Time, Time Source, Measure",["Time";"Time Source";"Measure"];
  "dziedziczenie roli po nadrzędniku, z tym, że z atrybutem Background",["dziedziczona po nadrzędniku + Background"];
  "dziedziczy rolę po nadrzędniku",["dziedziczona po nadrzędniku"];
  "dziedziczenie roli po nadrzędniku + atrybut Background",["dziedziczona po nadrzędniku + Background"];
  "Manner, Attribute",["Manner";"Attribute"];
  "Measure, Manner",["Measure";"Manner"];
  "Theme, Theme Source",["Theme";"Theme Source"];
  "dziedziczy rolę tematyczną po nadrzędniku",["dziedziczona po nadrzędniku"];
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let clean_subentry_attribute get_selector set_selector translation semroles =
  Xlist.map semroles (fun (name,headings,tab) ->
    name,headings,List.rev (Xlist.rev_map tab (fun (e,l) ->
      {e with subentries=
        Xlist.map e.subentries (fun f ->
          set_selector f (List.flatten (Xlist.map (get_selector f) (fun s ->
            try StringMap.find translation s with Not_found -> [s]))))},l)))

let print_semroles semroles =
  Xlist.iter semroles (fun (name,headings,entries) ->
    Printf.printf "\n\n\n%s: %s\n\n" name (String.concat "; " headings);
    Xlist.iter entries (fun (e,entry) ->
      if Xlist.size entry > 1 then (
        print_entry e;
        Xlist.iter entry (fun line ->
          print_endline (String.concat "\t" (Xlist.map line (fun a -> a.value)))))))

let print_semroles2 semroles =
  Xlist.iter semroles (fun (name,headings,entries) ->
    Printf.printf "\n\n\n%s: %s\n\n" name (String.concat "; " headings);
    Xlist.iter entries (fun (e,entry) ->
      if Xlist.size entry > 1 then (
        print_entry e;
        Xlist.iter entry (fun l ->
        print_endline "---------------------------------";
        Xlist.iter l (fun line ->
          print_endline (String.concat "\t" (Xlist.map line (fun a -> a.value))))))))

let xml_of_entry e =
      Xml.Element("entry",["lexeme",e.lexeme;"pos",e.pos;"prep_sem",e.prep_sem;"mwe",e.mwe;"etymology",e.etymology;
                           "sjp_id1",e.sjp_id1;"sjp_id2",e.sjp_id2;"sjp_id3",e.sjp_id3],
        [Xml.Element("gloss",[],[Xml.PCData e.gloss])] @
        Xlist.map e.sem_phenomenon (fun x -> Xml.Element("sem_phenomenon",[],[Xml.PCData x])) @
        Xlist.map e.phenomenon_details (fun x -> Xml.Element("phenomenon_details",[],[Xml.PCData x])) @
        Xlist.map e.comp_type (fun x -> Xml.Element("comp_type",[],[Xml.PCData x])) @
        Xlist.map e.sentence_type (fun x -> Xml.Element("sentence_type",[],[Xml.PCData x])) @
        Xlist.map e.comments (fun x -> Xml.Element("comment",[],[Xml.PCData x])) @
        Xlist.map e.primary_pos (fun x -> Xml.Element("primary_pos",[],[Xml.PCData x])) @
        Xlist.map e.dependent_pos (fun x -> Xml.Element("dependent_pos",[],[Xml.PCData x])) @
        Xlist.map e.dependent_case (fun x -> Xml.Element("dependent_case",[],[Xml.PCData x])) @
        Xlist.map e.coordination (fun x -> Xml.Element("coordination",[],[Xml.PCData x])) @
        Xlist.map e.subentries (fun s -> Xml.Element("subentry",[],
          Xlist.map s.thematic_role (fun x -> Xml.Element("thematic_role",[],[Xml.PCData x])) @
          Xlist.map s.primary_selprefs (fun x -> Xml.Element("primary_selprefs",[],[Xml.PCData x])) @
          Xlist.map s.dependent_selprefs (fun x -> Xml.Element("dependent_selprefs",[],[Xml.PCData x])) @
          Xlist.map s.examples (fun x -> Xml.Element("examples",[],[Xml.PCData x])) @
          Xlist.map s.examples_sources (fun x -> Xml.Element("examples_sources",[],[Xml.PCData x])))))

let xml_of semroles =
  Xml.Element("dictionaries",[],Xlist.map semroles (fun (name,entries) ->
    Xml.Element("dictionary",["name",name],List.rev (Xlist.rev_map entries xml_of_entry))))

let print_summary name selector semroles =
  let qmap = Xlist.fold semroles StringQMap.empty (fun qmap (_,tab) ->
    Xlist.fold tab qmap (fun qmap e ->
      Xlist.fold (selector e) qmap StringQMap.add)) in
  let l = Xlist.sort (StringQMap.fold qmap [] (fun l k v -> (v,k) :: l)) compare in
  Printf.printf "%s\n" name;
  Xlist.iter l (fun (v,s) ->
    Printf.printf "%4d %s\n" v s)

let print_summaries semroles =
  print_summary "pos" (fun e -> [e.pos]) semroles;
  print_summary "prep_sem" (fun e -> [e.prep_sem]) semroles;
  print_summary "comments" (fun e -> e.comments) semroles;
  print_summary "primary_pos" (fun e -> e.primary_pos) semroles;
  print_summary "dependent_pos" (fun e -> e.dependent_pos) semroles;
  print_summary "dependent_case" (fun e -> e.dependent_case) semroles;
  print_summary "coordination" (fun e -> e.coordination) semroles;
  print_summary "sem_phenomenon" (fun e -> e.sem_phenomenon) semroles;
  print_summary "phenomenon_details" (fun e -> e.phenomenon_details) semroles;
  print_summary "comp_type" (fun e -> e.comp_type) semroles;
  print_summary "sentence_type" (fun e -> e.sentence_type) semroles;
  print_summary "mwe" (fun e -> [e.mwe]) semroles;
  print_summary "etymology" (fun e -> [e.etymology]) semroles;
  print_summary "thematic_role" (fun e -> List.flatten (Xlist.map e.subentries (fun s -> s.thematic_role))) semroles;
  print_summary "primary_selprefs" (fun e -> List.flatten (Xlist.map e.subentries (fun s -> s.primary_selprefs))) semroles;
  print_summary "dependent_selprefs" (fun e -> List.flatten (Xlist.map e.subentries (fun s -> s.dependent_selprefs))) semroles;
  print_summary "examples_sources" (fun e -> List.flatten (Xlist.map e.subentries (fun s -> s.examples_sources))) semroles

let predef_prefs = StringSet.of_list [
  "ALL";"LUDZIE";"ISTOTY";"PODMIOTY";"JADŁO";"DOBRA";
  "KOMUNIKAT";"KONCEPCJA";"WYTWÓR";"POŁOŻENIE";"MIEJSCE";
  "OTOCZENIE";"CZAS";"OBIEKTY";"CECHA";"CZYNNOŚĆ";"SYTUACJA";
  "KIEDY";"CZEMU";"ILOŚĆ"]

let map_selpref s =
  match Xstring.split " - " s with
    [s1;s2;variant] -> if StringMap.mem !ENIAMplWordnet.lumap (s1 ^ " - " ^ s2) then s else (print_endline s; s)
  | [s;variant] -> if StringMap.mem !ENIAMplWordnet.lumap s then s else (print_endline s; s)
  | [s] -> if StringSet.mem predef_prefs s then s else (print_endline s; s)
  | _ -> print_endline ("map_selpref: " ^ s); s

let map_selprefs e =
  {e with subentries=Xlist.map e.subentries (fun f ->
    {f with
      primary_selprefs=Xlist.map f.primary_selprefs map_selpref;
      dependent_selprefs=Xlist.map f.dependent_selprefs map_selpref})}

let load_semroles () =
  (* ENIAMplWordnet.initialize (); *)
  let semroles = load_data semroles_filename in
  let semroles = Xlist.map semroles (fun (name,tab) -> name, split_into_entries tab) in
  let semroles = Xlist.map semroles split_into_entries_additional in
  let semroles = Xlist.map semroles (fun (name,tab) -> name, remove_empty_entries tab) in
  let semroles = Xlist.map semroles (fun (name,tab) ->
    let headings, tab = extact_headings tab in
    (* Printf.printf "%s: %s\n\n" name (String.concat "; " headings); *)
    name, headings, tab) in
  let semroles = Xlist.map semroles (fun (name,headings,entries) ->
    name,headings,List.rev (Xlist.rev_map entries (fun e -> empty_entry,e))) in
  let semroles = Xlist.map semroles (remove_empty_column ["L.p."]) in
  let semroles = Xlist.map semroles (extract_single_value
    ["Przyimek";"Wyrażenie przyimkowe";"Kublik";"Spójnik"]
    (fun e s -> {e with lexeme=s})) in
  let semroles = Xlist.map semroles (extract_single_value
    ["Klasa fleksemów"]
    (fun e s -> {e with pos=s})) in
  let semroles = Xlist.map semroles (extract_single_value
    ["Semantyka przyimka";"Semantyka"]
    (fun e s -> {e with prep_sem=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["rodzaj zdania"]
    (fun e s -> {e with sentence_type=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Rodzaj spójnika"]
    (fun e s -> {e with comp_type=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Zjawisko semantyczne"]
    (fun e s -> {e with sem_phenomenon=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Szczegóły zjawiska"]
    (fun e s -> {e with phenomenon_details=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Uwagi"]
    (fun e s -> {e with comments=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Część mowy jaką jest nadrzędnik"; "Część mowy, jaką jest nadrzędnik"]
    (fun e s -> {e with primary_pos=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Część mowy jaką jest podrzędnik"; "Część mowy, jaką jest podrzędnik"]
    (fun e s -> {e with dependent_pos=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Przypadki podrzędnika (jeżeli się odmienia)"]
    (fun e s -> {e with dependent_case=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["Koordynacja (jeżeli podrzędnik się nie deklinuje)"]
    (fun e s -> {e with coordination=s})) in
  let semroles = Xlist.map semroles (fun (name,headings,tab) -> name, headings,
    List.rev (Xlist.rev_map tab merge_subentries)) in
  let semroles = Xlist.map semroles (extract_subentry_value
    ["Rola tematyczna"]
    (fun e s -> {e with thematic_role=s})) in
  let semroles = Xlist.map semroles (extract_single_subentry_value
    ["Uwagi"]
    (fun e s -> {e with comments=[s]})) in
  let semroles = Xlist.map semroles (extract_subentry_value
    ["Synonim"]
    (fun e s -> if s = [] then e else failwith "Synonim")) in
  let semroles = Xlist.map semroles (fun (name,headings,tab) -> name, headings,
    if name = "Kubliki" then tab else
    List.rev (Xlist.rev_map tab split_subentries)) in
  let semroles = Xlist.map semroles (extract_subentry_value
    ["Preferencje selekcyjne nadrzędnika"]
    (fun e s -> {e with primary_selprefs=s})) in
  let semroles = Xlist.map semroles (fun (name,headings,tab) -> name, headings,
    List.rev (Xlist.rev_map tab split_subentries)) in
  let semroles = Xlist.map semroles (extract_subentry_value
    ["Preferencje selekcyjne podrzędnika"]
    (fun e s -> {e with dependent_selprefs=s})) in
  let semroles = Xlist.map semroles (extract_single_subentry_value
    ["Słowosieć?";"SGJP"]
    (fun e s -> {e with sjp_id1=s})) in
  let semroles = Xlist.map semroles (extract_single_subentry_value
    ["Nr Sem.";"Nr. Sem"]
    (fun e s -> {e with sjp_id2=s})) in
  let semroles = Xlist.map semroles (extract_single_subentry_value
    ["przypadki"]
    (fun e s -> {e with sjp_id3=s})) in
  let semroles = Xlist.map semroles (extract_single_subentry_value
    ["Glosa";"Znaczenie"]
    (fun e s -> {e with gloss=s})) in
  let semroles = Xlist.map semroles (extract_subentry_value
    ["Przykłady"]
    (fun e s -> {e with examples=s})) in
  let semroles = Xlist.map semroles (extract_subentry_value
    ["Źródło przykładów"]
    (fun e s -> {e with examples_sources=s})) in
  let semroles = clean_subentry_attribute
    (fun e -> e.thematic_role) (fun e s -> {e with thematic_role=s})
    thematic_role_translation semroles in
  (* print_semroles2 semroles; *)
  let semroles = Xlist.map semroles (fun (name,headings,entries) ->
    name,List.rev (Xlist.rev_map entries (fun (e,_) -> e))) in
  (* let semroles = Xlist.map semroles (fun (name,entries) ->
    name,List.rev (Xlist.rev_map entries map_selprefs)) in *)
  semroles

let load_interj () =
  let semroles = load_data interj_filename in
  let semroles = match semroles with
      ["",tab] -> ["Wykrzykniki",List.rev (Xlist.rev_map tab (fun line -> [line]))]
    | _ -> failwith "load_interj" in
  let semroles = Xlist.map semroles (fun (name,tab) -> name, remove_empty_entries tab) in
  let semroles = Xlist.map semroles (fun (name,tab) ->
    let headings, tab = extact_headings tab in
    (* Printf.printf "%s: %s\n\n" name (String.concat "; " headings); *)
    name, headings, tab) in
  let semroles = Xlist.map semroles (fun (name,headings,entries) ->
    name,headings,List.rev (Xlist.rev_map entries (fun e -> empty_entry,e))) in
  let semroles = Xlist.map semroles (extract_single_value
    ["wykrzyknik"]
    (fun e s -> {e with lexeme=s})) in
  let semroles = Xlist.map semroles (extract_multiple_value
    ["znaczenie"]
    (fun e s -> {e with sem_phenomenon=s})) in
  let semroles = Xlist.map semroles (extract_single_value
    ["pochodzenie (właściwe/pochodne/wyrażenie przyimkowe)"]
    (fun e s -> {e with etymology=s})) in
  let semroles = Xlist.map semroles (extract_single_value
    ["wyrażenia przyimkowe/wykrzykniki występujące w stałych zwrotach"]
    (fun e s -> {e with mwe=s})) in
  let semroles = Xlist.map semroles (extract_single_value
    ["źródło"]
    (fun e s -> {e with pos=s})) in
  (* print_semroles2 semroles; *)
  let semroles = Xlist.map semroles (fun (name,headings,entries) ->
    name,List.rev (Xlist.rev_map entries (fun (e,_) -> e))) in
  semroles

let is_sem e =
  match e.prep_sem with
    "" -> true
  | "semantyczny" -> true
  | "niesemantyczny" -> false
  | "silnie uzależnione od czasownika" -> false
  | s -> print_endline ("is_sem: " ^ Xml.to_string_fmt (xml_of_entry e)); false

let make_argument e = function
    [],[] -> "null"
  | ["noun"],[] -> print_endline ("make_argument: [noun] []"); ""
  | ["noun"],l -> String.concat "+" (Xlist.map l (fun c -> "np(" ^ c ^ ")"))
  | ["noun";"ger"],l -> String.concat "+" (Xlist.map l (fun c -> "np(" ^ c ^ ")"))
  | ["ger";"noun"],l -> String.concat "+" (Xlist.map l (fun c -> "np(" ^ c ^ ")"))
  | ["ger"],l -> String.concat "+" (Xlist.map l (fun c -> "np(" ^ c ^ ")"))
  | s,t -> print_endline ("make_argument: " ^ e.lexeme ^ " [" ^ String.concat ";" s ^ "] [" ^ String.concat ";" t ^ "]"); ""

type lex_entry = {lemma: string; pos1: string; pos2: string; phrase: string;
  others: (string * string list) list;
  argument: string; modifier: string; selprefs: string list; omit: bool}

let empty_lex_entry = {lemma=""; pos1=""; pos2="";
  phrase=""; others=[]; argument=""; modifier=""; selprefs=[]; omit=false}

let rec clean_others = function
    (a,l) :: others ->
      let l = List.flatten (Xlist.map l (function "" -> [] | s -> [s])) in
      if l = [] then clean_others others else (a,l) :: (clean_others others)
  | [] -> []

let render_valence semroles =
  Xlist.fold semroles [] (fun l (name,tab) ->
    let p0 = match name with
        "Przyimki SJP" | "Przyimki spoza SJP" | "Wyrażenia przyimkowe" ->
           {empty_lex_entry with pos1="x"; pos2="prep"; phrase="xp";
            modifier="\\{null+advp+qubp}: Attr[GenericDescription]"}
      | "Kubliki" -> {empty_lex_entry with pos2="qub"}
      | "Spójniki podrzędne" ->
           {empty_lex_entry with (*pos1="x";*) pos2="comp"; phrase="xp";
            argument="ip+infp"}
      | "Spójniki współrzędne" -> {empty_lex_entry with omit=true}
      | "Wykrzykniki" -> {empty_lex_entry with pos2="interj"}
      | _ -> print_endline ("render_valence: " ^ name); empty_lex_entry in
    if p0.omit then l else
    Xlist.fold tab l (fun l e ->
      if is_sem e then
        let argument =
          if p0.argument <> "" && e.dependent_pos=[] && e.dependent_case=[] then p0.argument
          else make_argument e (e.dependent_pos,e.dependent_case) in
        let p1 = if argument = "" then {p0 with omit=true} else p0 in
        let others =
          ["pos", [e.pos]; "comments",e.comments;
           "primary_pos",e.primary_pos; "coordination",e.coordination;
           "sjp_id",[String.concat " " (List.flatten (Xlist.map [e.sjp_id1;e.sjp_id2;e.sjp_id3] (function "" -> [] | s -> [s])))];
           "gloss",[e.gloss]; "sem_phenomenon",e.sem_phenomenon; "phenomenon_details",e.phenomenon_details;
           "comp_type",e.comp_type; "sentence_type",e.sentence_type;
           "etymology",[e.etymology]; "mwe",[e.mwe]] in
        let p1 = {p1 with lemma=e.lexeme; others=others; argument=argument} in
        Xlist.fold e.subentries l (fun l f ->
          let selprefs = f.dependent_selprefs in
          let others = p1.others @
            ["thematic_role",f.thematic_role; "primary_selprefs",f.primary_selprefs] @
            (Xlist.map f.examples (fun s -> "example",[s])) @
            ["examples_sources",f.examples_sources] in
          {p1 with selprefs=selprefs; others=clean_others others} :: l)
      else l))

let print_valence filename valence =
  let valence = Xlist.sort valence (fun a b ->
    if compare a.lemma b.lemma <> 0 then compare a.lemma b.lemma else
    let a_sjp_id = try Xlist.assoc a.others "sjp_id" with Not_found -> [] in
    let b_sjp_id = try Xlist.assoc b.others "sjp_id" with Not_found -> [] in
    compare a_sjp_id b_sjp_id) in
  File.file_out filename (fun file ->
    Xlist.iter valence (fun p ->
      let selectors =
        ["lemma",p.lemma;"pos2",p.pos2] @
        (if p.pos1="" then [] else ["pos",p.pos1]) @
        (if p.phrase="" then [] else ["phrase",p.phrase]) in
      let selectors = String.concat "," (Xlist.map selectors (fun (a,b) -> a ^ "=" ^ b)) in
      let positions =
        (if p.modifier="" then [] else [p.modifier]) @
        (if p.argument="null" then [] else
          [Printf.sprintf "/{%s}: Arg[%s]" p.argument (String.concat "," p.selprefs)]) in
      let positions = String.concat " *\n  " positions in
      let positions = if positions="" then " " else "\n  " ^ positions in
      Printf.fprintf file "%s: XX:%s;\n" selectors positions;
      Xlist.iter p.others (fun (a,l) ->
        Printf.fprintf file "#%s: %s\n" a (String.concat "; " l));
      Printf.fprintf file "\n"))

let _ =
  let semroles = load_semroles () in
  let interj = load_interj () in
  let semroles = semroles @ interj in
  let valence = render_valence semroles in
  print_valence "results/valence.dic" valence;
  File.file_out "results/dict.xml" (fun file ->
    Printf.fprintf file "%s" (Xml.to_string_fmt (xml_of semroles)));
  (* print_summaries semroles;  *)
  ()
