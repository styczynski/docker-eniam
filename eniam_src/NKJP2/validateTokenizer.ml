(*
 *  ENIAM_NKJP, an interface for National Corpus of Polish (NKJP).
 *  Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences
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

open ENIAMtokenizerTypes
open Xstd

type sent = SentBeg | SentEnd | Inside | SentBegEnd | Space

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
  set_beg_as_zero (List.rev (Xlist.fold sentences [] (fun l (id_s,tokens,named_tokens) ->
    set_sent_end (Xlist.fold tokens (l,SentBeg) (fun (l,sent) (beg,len,no_spaces,real_orth,orth,lemma,cat,interp) ->
      (sent,beg,len,no_spaces,real_orth,orth,lemma,cat,interp) :: l, Inside)))))

type ntoken = {nsent: sent; north: string; nlemma: string; ncat: string; ninterp: string list}

let space = {nsent=Space;north=" ";nlemma=" ";ncat="sp";ninterp=[]}

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

let rec simple_allign prev_orth prev_cat rev = function
    (SentBeg,0,_,_,_,orth,lemma,cat,interp) :: l ->
       simple_allign orth cat ({nsent=SentBeg;north=orth;nlemma=lemma;ncat=cat;ninterp=interp} :: rev) l
  | (SentBegEnd,0,_,_,_,orth,lemma,cat,interp) :: l ->
       simple_allign orth cat ({nsent=SentBegEnd;north=orth;nlemma=lemma;ncat=cat;ninterp=interp} :: rev) l
  | (_,0,_,_,_,orth,lemma,cat,interp) :: l -> failwith ("allign 1: " ^ orth)
  | (sent,beg,_,no_spaces,_,orth,lemma,cat,interp) :: l ->
       let rev =
         if no_spaces > 0 then space :: rev else
         if is_space_required prev_orth prev_cat orth cat then space :: rev else rev in
       simple_allign orth cat ({nsent=sent;north=orth;nlemma=lemma;ncat=cat;ninterp=interp} :: rev) l
  | [] -> List.rev rev

let render_paragraph tokens =
  String.concat "" (List.rev (Xlist.rev_map tokens (fun n -> n.north)))

let rec remove_spaces rev = function
    {nsent=Space} :: l -> remove_spaces rev l
  | x :: l -> remove_spaces (x :: rev) l
  | [] -> List.rev rev

type vtoken =
    T of token_env
  | V of token_env list list
  | D of token_env list list

let string_of_vtoken = function
    T t -> Printf.sprintf "T(%s)" t.orth
  | V(tl) ->
      Printf.sprintf "V[ %s ]"
        (String.concat " | " (Xlist.map tl (fun l -> String.concat " " (Xlist.map l (fun t -> t.orth)))))
  | D(tl) ->
      Printf.sprintf "D[ %s ]"
        (String.concat " | " (Xlist.map tl (fun l -> String.concat " " (Xlist.map l (fun t -> t.orth)))))

let rec convert_eniam_tokens_rec = function
    Token t -> [t]
  | Seq l -> List.flatten (List.rev (Xlist.rev_map l convert_eniam_tokens_rec))
  | Variant l -> failwith "convert_eniam_tokens_rec"

let rec convert_eniam_tokens rev = function
    Token t :: l -> convert_eniam_tokens (T t :: rev) l
  | Seq l2 :: l -> convert_eniam_tokens rev (l2 @ l)
  | Variant [t] :: l -> convert_eniam_tokens rev (t :: l)
  | Variant l2 :: l -> let l2 = Xlist.map l2 convert_eniam_tokens_rec in convert_eniam_tokens (V l2 :: rev) l
  | [] -> List.rev rev

let orth_string_of_eniam_seq l =
  String.concat " " (List.flatten (Xlist.map l (fun t ->
    if t.orth = "" then [] else [t.orth])))

let orth_string_of_eniam_seq2 l =
  String.concat "\";\"" (List.flatten (Xlist.map l (fun t ->
    if t.orth = "" then [] else [t.orth])))

let string_of_eniam_token_orths l =
  String.concat " " (List.flatten (Xlist.map l (function
      T{orth=""} -> []
    | T t -> [t.orth]
    | V variants | D variants ->
      let set = Xlist.fold variants StringSet.empty (fun set seq -> StringSet.add set (orth_string_of_eniam_seq seq)) in
      match StringSet.to_list set with
        [s] -> [s]
      | l -> ["[ " ^ String.concat " | " l ^ " ]"])))

let string_of_eniam_token_orths2 l =
  String.concat "\";\"" (List.flatten (Xlist.map l (function
      T{orth=""} -> []
    | T t -> [t.orth]
    | V variants | D variants ->
      let set = Xlist.fold variants StringSet.empty (fun set seq -> StringSet.add set (orth_string_of_eniam_seq2 seq)) in
      match StringSet.to_list set with
        [s] -> [s]
      | l -> ["[ \"" ^ String.concat "\" | \"" l ^ "\" ]"])))

type variant_classes = Disamb | Amb | Unknown

let classify_variants l =
  match Xlist.map l (Xstring.split " ") with
    [[a];[b;"m"]] -> if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "m" then Disamb else Unknown
  | [[a];[b;"em"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "em" then Disamb else Unknown
  | [[a];[b;"ż"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "ż" then Disamb else Unknown
  | [[a];[b;"ń"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "ń" then Disamb else Unknown
  | [[a];[b;"ś"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "ś" then Disamb else Unknown
  | [[a];[b;"by"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "by" then Disamb else Unknown
  | [[a];[b;"m"];[c;"em"]] ->
           if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "m" &&
              String.sub b 0 (String.length b - 1) = c && String.sub b (String.length b - 1) 1 = "e" then Disamb else Unknown
  | [[a];[b;"ś"];[c;"eś"]] ->
           if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "ś" &&
              String.sub b 0 (String.length b - 1) = c && String.sub b (String.length b - 1) 1 = "e" then Disamb else Unknown
  | [[a];[b;"śmy"];[c;"eśmy"]] ->
           if String.sub a 0 (String.length a - 4) = b && String.sub a (String.length a - 4) 4 = "śmy" &&
              String.sub b 0 (String.length b - 1) = c && String.sub b (String.length b - 1) 1 = "e" then Disamb else Unknown
  | [[a];[b;"ście"];[c;"eście"]] ->
           if String.sub a 0 (String.length a - 5) = b && String.sub a (String.length a - 5) 5 = "ście" &&
              String.sub b 0 (String.length b - 1) = c && String.sub b (String.length b - 1) 1 = "e" then Disamb else Unknown
  | [[a];[b;"że"]] -> if String.sub a 0 (String.length a - 3) = b && String.sub a (String.length a - 3) 3 = "że" then Disamb else Unknown
  | [[a];[b;"śmy"]] -> if String.sub a 0 (String.length a - 4) = b && String.sub a (String.length a - 4) 4 = "śmy" then Disamb else Unknown
  | [[a];[b;"ście"]] -> if String.sub a 0 (String.length a - 5) = b && String.sub a (String.length a - 5) 5 = "ście" then Disamb else Unknown
  | [[a];[b;"m"];[c;"by";"m"]] ->
           if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "m" &&
              String.sub b 0 (String.length b - 2) = c && String.sub b (String.length b - 2) 2 = "by" then Disamb else Unknown
  | [[a];[b;"ś"];[c;"by";"ś"]] ->
           if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "ś" &&
              String.sub b 0 (String.length b - 2) = c && String.sub b (String.length b - 2) 2 = "by" then Disamb else Unknown
  | [[a];[b;"śmy"];[c;"by";"śmy"]] ->
           if String.sub a 0 (String.length a - 4) = b && String.sub a (String.length a - 4) 4 = "śmy" &&
              String.sub b 0 (String.length b - 2) = c && String.sub b (String.length b - 2) 2 = "by" then Disamb else Unknown
  | [[a];[b;"ście"];[c;"by";"ście"]] ->
           if String.sub a 0 (String.length a - 5) = b && String.sub a (String.length a - 5) 5 = "ście" &&
              String.sub b 0 (String.length b - 2) = c && String.sub b (String.length b - 2) 2 = "by" then Disamb else Unknown
  | [[a];[b;"."]] -> if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "." then Amb else Unknown
  | [[c;a];[d;b;"."]] -> if c=d && String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "." then Amb else Unknown
  | [[e;c;a];[f;d;b;"."]] -> if e=f && c=d && String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "." then Amb else Unknown
  | [[a];[b;"M"]] -> if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "M" then Disamb else Unknown
  | [[a];[b;"Ż"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "Ż" then Disamb else Unknown
  | [[a];[b;"Ń"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "Ń" then Disamb else Unknown
  | [[a];[b;"Ś"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "Ś" then Disamb else Unknown
  | [[a];[b;"ŻE"]] -> if String.sub a 0 (String.length a - 3) = b && String.sub a (String.length a - 3) 3 = "ŻE" then Disamb else Unknown
  | [[a];[b;"ŚCIE"]] -> if String.sub a 0 (String.length a - 5) = b && String.sub a (String.length a - 5) 5 = "ŚCIE" then Disamb else Unknown
  | [[a];[b;"BY"]] -> if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "BY" then Disamb else Unknown
  | [[a];[b;"M"];[c;"EM"]] ->
           if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "M" &&
              String.sub b 0 (String.length b - 1) = c && String.sub b (String.length b - 1) 1 = "E" then Disamb else Unknown
  | [[a];[b;"Ś"];[c;"EŚ"]] ->
           if String.sub a 0 (String.length a - 2) = b && String.sub a (String.length a - 2) 2 = "Ś" &&
              String.sub b 0 (String.length b - 1) = c && String.sub b (String.length b - 1) 1 = "E" then Disamb else Unknown
  | [[a];[b;"M"];[c;"BY";"M"]] ->
           if String.sub a 0 (String.length a - 1) = b && String.sub a (String.length a - 1) 1 = "M" &&
              String.sub b 0 (String.length b - 2) = c && String.sub b (String.length b - 2) 2 = "BY" then Disamb else Unknown
  | [[":]"];[":";"]"]] -> Disamb
  | [["˝";"."];[".";"˝"]] -> Amb
  | [["“";"."];[".";"“"]] -> Amb
  | [["’’";"."];[".";"’’"]] -> Amb
  | [[".";"''"];["''";"."]] -> Amb
  | [["”";"."];[".";"”"]] -> Amb
  | [[".";"\""];["\"";"."]] -> Amb
  | _ -> Unknown

let rec count_variants_par name paragraph stats ets =
  Xlist.fold ets stats (fun stats -> function
      V variants ->
        let set = Xlist.fold variants StringSet.empty (fun set seq -> StringSet.add set (orth_string_of_eniam_seq seq)) in
        (match StringSet.to_list set with
          [_] -> stats
        | l ->
           let s = match classify_variants l with
               Disamb -> "disamb"
             | Amb -> "amb"
             | Unknown -> "[ " ^ String.concat " | " l ^ " ]" in
           StringQMap.add stats s)
    | _ -> stats)

let annotate_variants_par ets =
  List.rev (Xlist.rev_map ets (function
      V variants ->
        let set = Xlist.fold variants StringSet.empty (fun set seq -> StringSet.add set (orth_string_of_eniam_seq seq)) in
        (match StringSet.to_list set with
          [_] -> V variants
        | l ->
           (match classify_variants l with
               Disamb -> D variants
             | Amb -> V variants
             | Unknown -> failwith "annotate_variants_par 1"))
    | T t -> T t
    | D _ -> failwith "annotate_variants_par 2"))

let count_variants stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let tokens = flatten_sentences sentences in
      let tokens = simple_allign "" "" [] tokens in
      let paragraph = render_paragraph tokens in
      let eniam_tokens = ENIAMtokenizer.parse paragraph in
      let eniam_tokens = convert_eniam_tokens [] eniam_tokens in
      count_variants_par name paragraph stats eniam_tokens))

type atoken =
    AT of token_env * ntoken list
  | AV of token_env list list * ntoken list
  | AR of string * token_env list list * ntoken list

let string_of_nkjp_token_orths l =
  String.concat " " (Xlist.map l (fun n -> n.north))

let string_of_nkjp_token_orths2 l =
  String.concat "\";\"" (Xlist.map l (fun n -> n.north))

let string_of_atoken = function
    AT(t,l) -> Printf.sprintf "AT(%s,[%s])" t.orth (string_of_nkjp_token_orths l)
  | AV(tl,l) ->
      Printf.sprintf "AV([ %s ],[%s])"
        (String.concat " | " (Xlist.map tl (fun l -> String.concat " " (Xlist.map l (fun t -> t.orth)))))
        (string_of_nkjp_token_orths l)
  | AR(stat,tl,l) ->
      Printf.sprintf "AR(%s,[ %s ],[%s])" stat
        (String.concat " | " (Xlist.map tl (fun l -> String.concat " " (Xlist.map l (fun t -> t.orth)))))
        (string_of_nkjp_token_orths l)

let rec get_first_rec = function
    AT(t,_) :: l -> [[t]] :: get_first_rec l
  | AV(tl,_) :: l -> tl :: get_first_rec l
  | [] -> [[[]]]
  | _ -> failwith "get_first_rec"

let get_first l =
  Xlist.rev_map (Xlist.multiply_list (get_first_rec l)) List.flatten

let rec get_second = function
    AT(_,m) :: l -> m @ (get_second l)
  | AV(_,m) :: l -> m @ (get_second l)
  | AR(_,_,m) :: l -> m @ (get_second l)
  | [] -> []
  (* | _ -> failwith "get_second" *)

let rec match_variants variants l =
  List.rev (Xlist.fold variants [] (fun res seq ->
    try
      let seq,l = Xlist.fold seq ([],l) (fun (seq,l) t ->
        let t,l = match_token (T t,l) in
        seq @ t, l) in
      (seq,l) :: res
    with Not_found -> res))

and match_token = function
    T({orth=""} as t), l -> [AT(t,[])], l
  | T t, n :: l ->
      if t.orth = n.north then [AT(t,[n])], l else raise Not_found
  | T t, [] -> raise Not_found
  | V variants, l ->
      (match match_variants variants l with
        [] -> raise Not_found
      | (seq,l) :: _ -> [AV(variants,get_second seq)],l)
  | D variants, l ->
      (match match_variants variants l with
        [] -> raise Not_found
      | [seq,l] -> seq,l
      | ((m,l) :: _) as variants ->
           let l2 = get_second m in
           [AV(List.flatten (Xlist.map variants (fun (m,_) -> get_first m)),l2)],l)

let rec match_token_list rev = function
    ets,[] -> List.flatten (List.rev rev), ets
  | [],_ -> raise Not_found
  | t :: ets, l ->
      let a,l = match_token (t,l) in
      match_token_list (a :: rev) (ets,l)

let rec get_tokens_orth = function
    T t -> t.orth
  | V(seq :: _) -> String.concat "" (Xlist.map seq (fun t -> t.orth))
  | D(seq :: _) -> String.concat "" (Xlist.map seq (fun t -> t.orth))
  | _ -> failwith "get_tokens_orth"

let remove_spaces_string s =
  String.concat "" (Xstring.split " " s)

let rec combine e_pref n_pref e_rev n_rev ets l =
  (* Printf.printf "combine e_pref=%s n_pref=%s\n%!" e_pref n_pref; *)
  if String.length e_pref < String.length n_pref then
    if ets = [] then List.rev e_rev, List.rev n_rev @ l, [], [] else
    let t = List.hd ets in
    let s = remove_spaces_string (get_tokens_orth t) in
    combine (e_pref ^ s) n_pref (t :: e_rev) n_rev (List.tl ets) l
  else if String.length e_pref > String.length n_pref then
    if l = [] then List.rev e_rev @ ets, List.rev n_rev, [], [] else
    let n = List.hd l in
    combine e_pref (n_pref ^ remove_spaces_string n.north) e_rev (n :: n_rev) ets (List.tl l)
  else if e_pref = "" then
    if ets = [] then List.rev e_rev, List.rev n_rev @ l, [], [] else
    if l = [] then List.rev e_rev @ ets, List.rev n_rev, [], [] else
    let t = List.hd ets in
    let s = remove_spaces_string (get_tokens_orth t) in
    let n = List.hd l in
    combine s (remove_spaces_string n.north) (t :: e_rev) (n :: n_rev) (List.tl ets) (List.tl l)
  else List.rev e_rev, List.rev n_rev, ets, l

let prepare_e_pat s =
  {nsent=Space;north=s;nlemma="";ncat="";ninterp=[]}

let load_rules filename =
  List.rev (File.fold_tab filename [] (fun l -> function
    [e;n] -> (Xlist.map (Xstring.split " " e) prepare_e_pat, Xstring.split " " n) :: l
  | _ -> failwith "load_rules"))

let space_rules = Xlist.map [
  "nkjp-correct",["'99 - 26"],["'99";"-";"26"];
  "both-correct",["1 VII 1945"],["1";"VII";"1945"];
  "both-correct",["1 VII 1960"],["1";"VII";"1960"];
  "both-correct",["1,50 - 2"],["1";",";"50";"-";"2"];
  "both-correct",["10 - 17"],["10";"-";"17"];
  "both-correct",["10.00 – 15.00"],["10";".";"00";"–";"15";".";"00"];
  "both-correct",["10:10 - 13:15"],["10";":";"10";"-";"13";":";"15"];
  "nkjp-correct",["11 - 2001"],["11";"-";"2001"];
  "both-correct",["136 - 159"],["136";"-";"159"];
  "both-correct",["14 XII 1920"],["14";"XII";"1920"];
  "both-correct",["1500 - 800"],["1500";"-";"800"];
  "both-correct",["16 III 1945"],["16";"III";"1945"];
  "both-correct",["1973 - 1976"],["1973";"-";"1976"];
  "both-correct",["1981 - 1985"],["1981";"-";"1985"];
  "both-correct",["1998 - 1999"],["1998";"-";"1999"];
  "nkjp-correct",["2003 - 18"],["2003";"-";"18"];
  "both-correct",["22 - 24"],["22";"-";"24"];
  "both-correct",["24 I 1945"],["24";"I";"1945"];
  "both-correct",["24 III 1945"],["24";"III";"1945"];
  "both-correct",["24 VIII 1985"],["24";"VIII";"1985"];
  "both-correct",["25 - 30"],["25";"-";"30"];
  "eniam-correct",["261";"ha"],["261 ha"];
  "both-correct",["27 I 1945"],["27";"I";"1945"];
  "both-correct",["29 IV 1863"],["29";"IV";"1863"];
  "nkjp-correct",["3 - 2";"+";"4"],["3";"-";"2";"+";"4"];
  "both-correct",["3 - 4"],["3";"-";"4"];
  "both-correct",["300 - 400"],["300";"-";"400"];
  "both-correct",["31 I 1945"],["31";"I";"1945"];
  "nkjp-correct",["31";",";"6"],["31, 6"];
  "eniam-correct",["396 093"],["396";"093"];
  "both-correct",["4 VIII 1904"],["4";"VIII";"1904"];
  "both-correct",["5 - 8"],["5";"-";"8"];
  "eniam-correct",["510 256 732"],["510";"256";"732"];
  "eniam-correct",["517 193"],["517";"193"];
  "nkjp-correct",["52-52";"c"],["52";"-";"52c"];
  "both-correct",["6 - 11"],["6";"-";"11"];
  "nkjp-correct",["6 - 2"],["6";"-";"2"];
  "both-correct",["6 - 6,5"],["6";"-";"6,5"];
  "both-correct",["8.00 - 14.00"],["8";".";"00";"-";"14";".";"00"];
  "both-correct",["8.00 - 16.00"],["8";".";"00";"-";"16";".";"00"];
  "both-correct",["9 - 11"],["9";"-";"11"];
  "both-correct",["9 IV 1241"],["9";"IV";"1241"];
  "nkjp-correct",["K";"6-3 400"],["K6-3";"400"];
  "both-correct",["XVI - XVIII"],["XVI";"-";"XVIII"];
  "nkjp-correct",["ai";"pi";"si";"si"],["ai pi si si"];
  "nkjp-correct",["ce";"o";"dwa"],["ce o dwa"];
  "nkjp-correct",["co";"najmniej"],["co najmniej"];
  "nkjp-correct",["co";"najwyżej"],["co najwyżej"];
  "nkjp-correct",["dżi";"pi";"es"],["dżi pi es"];
  "both-correct",["m";"2"],["m 2"];
  "both-correct",["m";"3"],["m 3"];
  "both-correct",["m.";"in."],["m. in."];
  "nkjp-correct",["te";"fał";"en"],["te fał en"];
  "nkjp-correct",["te";"fał";"enu"],["te fał enu"];
  "nkjp-correct",["te";"fał";"pe"],["te fał pe"];
  "nkjp-correct",["techend";"trejt";"loj";"kropka";"bloks";"pot";"kom"],["techend trejt loj kropka bloks pot kom"];
  "nkjp-correct",["tik";"taka"],["tik taka"];
  "nkjp-correct",["w";"w";"w";"polskie";"radio";"euro";"kropka";"pe";"el"],["w w w polskie radio euro kropka pe el"];
  "nkjp-correct",["à";"la"],["à la"];
  "nkjp-correct",["à";"propos"],["à propos"];
  "both-correct",["m.";"in."],["m. in";"."];
  "nkjp-correct",["PM";"63"],["PM 63"];
  "both-correct",["R";".";"P";"."],["R. P."];
  "nkjp-correct",["1950 - 54.547"],["1950";"-";"54.547"];
  "nkjp-correct",["1950 - 82.756"],["1950";"-";"82.756"];
  "both-correct",["0,8 - 2,0"],["0,8";"-";"2,0"];
  "both-correct",["100 - 200"],["100";"-";"200"];
  "both-correct",["15 - 17"],["15";"-";"17"];
  "nkjp-correct",["2004 - 17,5"],["2004";"-";"17,5"];
  "nkjp-correct",["2005 - 17"],["2005";"-";"17"];
  "eniam-correct",["30";"m"],["30 m"];
  "nkjp-correct",["K";"6-2 400"],["K6-2";"400"];
  "nkjp-correct",["WIG";"20"],["WIG 20"];
  "eniam-correct",["Z";"DALA"],["Z DALA"];
  "nkjp-correct",["ha";"de"],["ha de"];
  "nkjp-correct",["o";"em"],["o em"];
  "nkjp-correct",["pe";"ka";"o"],["pe ka o"];
  "nkjp-correct",["pe";"pe";"en"],["pe pe en"];
  "both-correct",["w";".";"c";"."],["w. c";"."];
  "nkjp-correct",["jor";"self"],["jor self"];
  "both-correct",["7.30 - 15.30"],["7";".";"30";"-";"15";".";"30"];
  "both-correct",["(032) 51 30 86"],["(";"032";")";"51";"30";"86"];
  "both-correct",["0-46 855-45-26"],["0-46";"855-45-26"];
  "both-correct",["02-651"],["02";"-";"651"];
  "both-correct",["10-11 mld"],["10";"-";"11";"mld"];
  "nkjp-correct",["1:100";"000"],["1";":";"100 000"];
  "both-correct",["2,5-3 mln"],["2,5";"-";"3";"mln"];
  "both-correct",["22.12. - 20.01"],["22";".";"12";".";"-";"20";".";"01"];
  "both-correct",["30-40 mln"],["30";"-";"40";"mln"];
  "both-correct",["40-50 tys."],["40";"-";"50";"tys";"."];
  "both-correct",["70-75 tys."],["70";"-";"75";"tys";"."];
  "both-correct",["51 86 28"],["51";"86";"28"];
  "both-correct",["517 193"],["517";"193"];
  "both-correct",["100 mln."],["100";"mln";"."];
  "both-correct",["518 609"],["518";"609"];
  "eniam-correct",["cm";"3"],["cm 3"];
  "both-correct",["(071) 73-65-32"],["(";"071";")";"73-65-32"];
  "both-correct",["(601) 71 99 94"],["(";"601";")";"71";"99";"94"];
  "both-correct",["00-367"],["00";"-";"367"];
  "both-correct",["090 396 093"],["090";"396";"093"];
  "both-correct",["1 350 tys."],["1 350";"tys";"."];
  "both-correct",["100 - 200 tys."],["100";"-";"200";"tys";"."];
  "both-correct",["11 313,2 mln"],["11 313,2";"mln"];
  "both-correct",["3-4 mln"],["3";"-";"4";"mln"];
  "both-correct",["362 11 27"],["362";"11";"27"];
  "both-correct",["3–4 tys."],["3";"–";"4";"tys";"."];
  "both-correct",["4,5tys."],["4,5";"tys";"."];
  "both-correct",["421-30-26"],["421";"-";"30";"-";"26"];
  "both-correct",["429-56-29"],["429";"-";"56";"-";"29"];
  "both-correct",["635 78 40"],["635";"78";"40"];
  "both-correct",["635 79 10"],["635";"79";"10"];
  "both-correct",["695 22 11"],["695";"22";"11"];
  "both-correct",["78 10 14"],["78";"10";"14"];
  "both-correct",["15-20 tys."],["15";"-";"20";"tys";"."];
  "both-correct",["645 75 40"],["645";"75";"40"];
  "both-correct",["695 23 38"],["695";"23";"38"];
  "both-correct",["31";",";"6 mln"],["31, 6";"mln"];
  "both-correct",["1,9 mln";"."],["1,9";"mln";"."];
  "both-correct",["39 mln";"."],["39";"mln";"."];
  "both-correct",["54,4 mld";"."],["54,4";"mld";"."];
  "both-correct",["055 645 03 88"],["055";"645";"03";"88"];
  "nkjp-correct",["067 535";"-2"],["067";"535-2"];
  "both-correct",["091 418 6211";"."],["091";"418";"6211";"."];
  "eniam-correct",["pomaga";"my"],["pomaga my"];
  ] (fun (a,e,n) -> a, Xlist.map e prepare_e_pat, n)

let rules =
  Xlist.map (load_rules "data/brev.tab") (fun (e,n) -> "brev",e,n) @
  Xlist.map (load_rules "data/letni.tab") (fun (e,n) -> "letni",e,n) @
  Xlist.map (load_rules "data/both-correct.tab") (fun (e,n) -> "both-correct",e,n) @
  Xlist.map (load_rules "data/eniam-correct.tab") (fun (e,n) -> "eniam-correct",e,n) @
  Xlist.map (load_rules "data/nkjp-correct.tab") (fun (e,n) -> "nkjp-correct",e,n) @ space_rules

let rec match_n_pat rev = function
    s :: pat,n :: l -> if s = n.north then match_n_pat (n :: rev) (pat,l) else raise Not_found
  | [], l -> List.rev rev, l
  | _, [] -> raise Not_found

let disambiguate_letni = function
    AR("letni",variants,l) ->
      let variants = Xlist.fold variants [] (fun variants seq ->
        match List.rev seq with
          _ :: {token=Interp("-")} :: _ -> seq :: variants
        | _ -> variants) in
      let variants = Xlist.fold variants [] (fun variants seq ->
        match List.rev seq with
          _ :: _ :: {token=Dig(_,"intnum")} :: _ -> seq :: variants
        | _ :: _ :: {token=Compound("intnum-interval",_)} :: _ -> seq :: variants
        | _ :: _ :: {token=RomanDig(_,"roman")} :: _ -> seq :: variants
        | _ :: _ :: {token=Dig(_,"realnum")} :: _ -> seq :: variants
        | _ :: _ :: {token=Dig(_,"dig")} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"year")} :: _ -> variants
        | _ :: _ :: {token=Compound("year-interval",_)} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"month")} :: _ -> variants
        | _ :: _ :: {token=Compound("month-interval",_)} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"hour")} :: _ -> variants
        | _ :: _ :: {token=Compound("hour-interval",_)} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"day")} :: _ -> variants
        | _ :: _ :: {token=Compound("day-interval",_)} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"minute")} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"2dig")} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"pref3dig")} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"3dig")} :: _ -> variants
        | _ :: _ :: {token=Dig(_,"")} :: _ -> variants
        | _ :: _ :: {token=Proper(_,"obj-id",_,_)} :: _ -> variants
        | _ :: _ :: {token=RomanDig(_,"month")} :: _ -> variants
        | _ :: _ :: {token=AllCap _} :: _ -> variants
        | _ :: _ :: {token=FirstCap _} :: _ -> variants
        | _ :: _ :: {token=AllSmall _} :: _ -> variants
        | _ :: _ :: {token=CapLetter _} :: _ -> variants
        | _ :: _ :: {token=SmallLetter _} :: _ -> variants
(* Compound(intnum-interval,[Dig(2,intnum);Dig(3,intnum)])
Compound(day-interval,[Dig(2,day);Dig(3,day)])
Compound(month-interval,[Dig(2,month);Dig(3,month)])
Compound(year-interval,[Dig(2,year);Dig(3,year)])
Compound(hour-interval,[Dig(2,hour);Dig(3,hour)]) *)
        | _ :: _ :: t :: _ ->
            print_endline (ENIAMtokens.string_of_token t.token);
            seq :: variants
        | _ -> failwith "disambiguate_letni") in
      if variants = [] then failwith "disambiguate_letni: empty variants" else
      (* print_endline ("disambiguate_letni: " ^ string_of_atoken t);
      Xlist.iter variants (fun seq ->
        Xlist.iter seq (fun t -> print_endline (ENIAMtokens.string_of_token t.token));
        print_endline ""); *)
      AR("letni",variants,l)
  | t -> t

let rec apply_rules stats ets l = function
    [] -> raise Not_found
  | (stat,e_pat,n_pat) :: rules ->
      try
        let matched_n,l = match_n_pat [] (n_pat,l) in
        let matched_e,ets = match_token_list [] (ets,e_pat) in
        StringQMap.add stats stat, ets, l
      with Not_found -> apply_rules stats ets l rules

let rec annotate_apply_rules ets l = function
    [] -> raise Not_found
  | (stat,e_pat,n_pat) :: rules ->
      try
        let matched_n,l = match_n_pat [] (n_pat,l) in
        let matched_e,ets = match_token_list [] (ets,e_pat) in
        disambiguate_letni (AR(stat,get_first matched_e,matched_n)),ets,l
      with Not_found -> annotate_apply_rules ets l rules

let make_atoken l = function
    T t -> AR("tys",[[t]],l)
  | V variants -> AR("tys",variants,l)
  | _ -> failwith "make_atoken: ni"

let match_tys ets l =
  if ets = [] then raise Not_found else
  match Xstring.split " " (get_tokens_orth (List.hd ets)),l with
    [a;"tys."],x :: ({north="tys"} as y) ::({north="."} as z) ::l -> if a=x.north then make_atoken [x;y;z] (List.hd ets),List.tl ets,l else raise Not_found
  | [a;"tys"],x :: ({north="tys"} as y) ::l -> if a=x.north then make_atoken [x;y] (List.hd ets),List.tl ets,l else raise Not_found
  | [a;"mln"],x :: ({north="mln"} as y) ::l -> if a=x.north then make_atoken [x;y] (List.hd ets),List.tl ets,l else raise Not_found
  | [a;"mld"],x :: ({north="mld"} as y) ::l -> if a=x.north then make_atoken [x;y] (List.hd ets),List.tl ets,l else raise Not_found
  | _ -> raise Not_found

let rec match_and_combine name paragraph stats l = function
    et :: ets ->
      (try
        let _,l = match_token (et,l) in
        match_and_combine name paragraph stats l ets
      with Not_found -> (try
        let _,ets,l = match_tys (et :: ets) l in
        let stats = StringQMap.add stats "tys." in
        match_and_combine name paragraph stats l ets
      with Not_found -> (try
        let stats,ets,l = apply_rules stats (et :: ets) l rules in
        match_and_combine name paragraph stats l ets
      with Not_found ->
        let e_tokens,n_tokens,ets,l = combine "" "" [] [] (et :: ets) l in
        (* let stats = StringQMap.add stats (string_of_eniam_token_orths e_tokens ^ "\t" ^ string_of_nkjp_token_orths n_tokens ^ "\t" ^ name) in *)
        let stats = StringQMap.add stats (string_of_eniam_token_orths e_tokens ^ "\t" ^ string_of_nkjp_token_orths n_tokens ^ "\t" ^ paragraph) in
        (* let stats = StringQMap.add stats ("[\"" ^ string_of_eniam_token_orths2 e_tokens ^ "\"],[\"" ^ string_of_nkjp_token_orths2 n_tokens ^ "\"];" ^ "\t" ^ name) in *)
        (* let stats = StringQMap.add stats ("[\"" ^ string_of_eniam_token_orths2 e_tokens ^ "\"],[\"" ^ string_of_nkjp_token_orths2 n_tokens ^ "\"];" ^ "\t" ^ paragraph) in *)
        match_and_combine name paragraph stats l ets)))
  | [] -> if l = [] then stats else StringQMap.add stats ("match_and_combine: " ^ name ^ "\t" ^ string_of_nkjp_token_orths l ^ "\t" ^ paragraph)

let rec annotate_paragraph name paragraph l = function
    et :: ets ->
      (try
        (* print_endline ("annotate_paragraph 1: " ^ (string_of_vtoken et)); *)
        let m,l = match_token (et,l) in
        (* print_endline ("annotate_paragraph 2: " ^ (String.concat "\n" (Xlist.map m string_of_atoken))); *)
        m @ annotate_paragraph name paragraph l ets
      with Not_found -> (try
        let m,ets,l = match_tys (et :: ets) l in
        m :: annotate_paragraph name paragraph l ets
      with Not_found -> (try
        let m,ets,l = annotate_apply_rules (et :: ets) l rules in
        m :: annotate_paragraph name paragraph l ets
      with Not_found -> (*print_endline ("annotate_paragraph 1: " ^ (string_of_vtoken et));*)failwith "annotate_paragraph 1")))
  | [] -> if l = [] then [] else failwith "annotate_paragraph 2"

let validate_segmentation stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let tokens = flatten_sentences sentences in
      let tokens = simple_allign "" "" [] tokens in
      let paragraph = render_paragraph tokens in
      (* Printf.printf "rend:\t%s\n" paragraph; *)
      let tokens = remove_spaces [] tokens in
      let eniam_tokens = ENIAMtokenizer.parse paragraph in
      let eniam_tokens = convert_eniam_tokens [] eniam_tokens in
      match_and_combine name paragraph stats tokens eniam_tokens))

let check_annotation paragraph m =
    (* print_endline "check_annotation 1"; *)
  let paragraph = remove_spaces_string paragraph in
    (* print_endline "check_annotation 2"; *)
  let nkjp = remove_spaces_string (string_of_nkjp_token_orths (get_second m)) in
    (* print_endline "check_annotation 3"; *)
  if paragraph <> nkjp then failwith "check_annotation 1" else (
    (* print_endline "check_annotation 4"; *)
  Xlist.iter m (fun t ->
    (* print_endline "check_annotation 5"; *)
    let tl,l = match t with
        AT(t,l) -> [remove_spaces_string t.orth], l
      | AV(tl,l) | AR(_,tl,l) -> Xlist.rev_map tl (fun l -> String.concat "" (Xlist.map l (fun t -> remove_spaces_string t.orth))), l in
    let nkjp = remove_spaces_string (string_of_nkjp_token_orths l) in
    (* print_endline "check_annotation 6"; *)
    Xlist.iter tl (fun s -> if s <> nkjp then
      if s = ".''" && nkjp = "''." then () else
      if s = ".\"" && nkjp = "\"." then () else
      if s = ".˝" && nkjp = "˝." then () else
      if s = ".“" && nkjp = "“." then () else
      if s = ".’’" && nkjp = "’’." then () else
      if s = ".”" && nkjp = "”." then () else
      (*failwith*)print_endline ("check_annotation 2: " ^ s ^ " " ^ nkjp)));
    (* print_endline "check_annotation 8"; *)
  ())

let is_lemmatizable = function
  | AllSmall _ -> true
  | SmallLetter _ -> true
  | FirstCap _ -> true
  | AllCap _ -> true
  | CapLetter _ -> true
  | SomeCap _ -> true
  | t -> false

let set_sent sent t =
  match sent with
    SentBeg -> Token{t with attrs=SentBeg :: t.attrs}
  | SentEnd  -> Token{t with attrs=SentEnd :: t.attrs}
  | Inside -> Token t
  | SentBegEnd  -> Token {t with attrs=SentBegEnd :: t.attrs}
  | Space -> failwith "set_sent"

let set_sent_list ets l = (* FIXME: todo *)
  (* print_endline (String.concat " " (Xlist.map l (fun n ->
    match n.nsent with
      SentBeg -> "B"
    | SentEnd -> "E"
    | Inside -> "I"
    | SentBegEnd -> "BE"
    | Space -> "S"))); *)
  ets

let rec allign rev = function
    {orth=""} as t :: ets,nts -> allign ((t,[]) :: rev) (ets,nts)
  | [{orth="."} as x;{orth="''"} as y],[{north="''"};{north="."}] -> List.rev rev @ [x,[];y,[]]
  | [{orth="."} as x;{orth="\""} as y],[{north="\""};{north="."}] -> List.rev rev @ [x,[];y,[]]
  | [{orth="."} as x;{orth="˝"} as y],[{north="˝"};{north="."}] -> List.rev rev @ [x,[];y,[]]
  | [{orth="."} as x;{orth="“"} as y],[{north="“"};{north="."}] -> List.rev rev @ [x,[];y,[]]
  | [{orth="."} as x;{orth="’’"} as y],[{north="’’"};{north="."}] -> List.rev rev @ [x,[];y,[]]
  | [{orth="."} as x;{orth="”"} as y],[{north="”"};{north="."}] -> List.rev rev @ [x,[];y,[]]
  | t :: ets, n :: nts -> if t.orth = n.north then allign ((t,[n]) :: rev) (ets,nts) else ((*failwith*)print_endline ("allign 2: " ^ t.orth ^ " " ^ n.north); [])
  | [],[] -> List.rev rev
  | _ -> failwith "allign 3"

let transform_nkjp_interp cat interp1 =
  if interp1 = [] then [] else
  let interp = Xlist.map interp1 (fun s -> [s]) in
  match cat with
    "subst" | "ppron12" | "ppron3" | "ppas" | "pact" | "adj" | "num" | "depr" | "numcol" ->
       (match interp with
         ["sg"] :: case :: ["n"] :: l -> ["sg"] :: case :: ["n1";"n2"] :: l
       | ["pl"] :: case :: ["n"] :: l -> ["pl"] :: case :: ["n1";"n2";"p2";"p3"] :: l
       | ["pl"] :: case :: ["m1"] :: l -> ["pl"] :: case :: ["m1";"p1"] :: l
       | l -> l)
  | "ger" ->
       (match interp with
         num :: case :: ["n"] :: l -> num :: case :: ["n2"] :: l
       | l -> l)
  | "praet" | "winien" ->
       (match interp with
         ["sg"] :: ["n"] :: l -> ["sg"] :: ["n1";"n2"] :: l
       | ["pl"] :: ["n"] :: l -> ["pl"] :: ["n1";"n2";"p2";"p3"] :: l
       | ["pl"] :: ["m1"] :: l -> ["pl"] :: ["m1";"p1"] :: l
       | l -> l)
  | "prep" | "adv" | "fin" | "inf" | "imps" | "pcon" | "bedzie" | "impt" | "siebie" | "aglt" | "pant" | "brev" | "qub" -> interp
  | _ -> print_endline ("transform_nkjp_interp: " ^ cat ^ " " ^ String.concat ":" interp1); interp

let transform_nkjp_interp_simple cat interp1 =
  Xlist.map interp1 (fun s -> [s])

let merge_token = function
    t,[] -> Token t
  | t,[{ncat="brev"} as n] -> set_sent n.nsent {t with attrs=BrevLemma n.nlemma :: t.attrs}
  | t,[n] ->
      if n.nlemma = "+/-" then set_sent n.nsent t else
      if is_lemmatizable t.token then set_sent n.nsent {t with attrs=Disamb(n.nlemma,n.ncat,transform_nkjp_interp_simple n.ncat n.ninterp) :: t.attrs}
      else set_sent n.nsent t
  | _ -> failwith "merge_token"

let merge_letni l seq =
  if l = [] then failwith "merge_letni" else
  let n = List.hd (List.rev l) in
  let lemma = List.hd (List.rev (Xstring.split "-" n.nlemma)) in
  let seq = match seq with
      first :: l -> if n.nsent=SentBeg || n.nsent=SentBegEnd then {first with attrs=SentBeg :: first.attrs} :: l else first :: l
    | _ -> failwith "merge_letni" in
  match List.rev seq with
    last :: l ->
      let attrs = if n.nsent=SentEnd || n.nsent=SentBegEnd then (SentEnd : attr) :: last.attrs else last.attrs in
      Seq(Xlist.rev_map ({last with attrs=Disamb(lemma,n.ncat,transform_nkjp_interp_simple n.ncat n.ninterp) :: attrs} :: l) (fun t -> Token t))
  | _ -> failwith "merge_letni"

let blabla_orths = StringSet.of_list ["8.12"; "9.11"; "1.1"; "1.2"]

let is_blabla = function
    [{north=s};{north="."}] -> StringSet.mem blabla_orths s (*then (print_endline ("blabla: " ^ s); true) else false*)
  | _ -> false

let merge_paragraph name = function
    AT(t,l) -> merge_token (t,l)
  | AV(variants,l) ->
      if is_blabla l then Variant(Xlist.rev_map variants (fun ets -> Seq(Xlist.map (set_sent_list ets l) (fun t -> Token t)))) else
      Variant(Xlist.rev_map variants (fun ets -> Seq(Xlist.map (allign [] (ets,l)) merge_token)))
  | AR("tys",variants,l) -> Variant(Xlist.rev_map variants (fun ets -> Seq(Xlist.map (set_sent_list ets l) (fun t -> Token t))))
  | AR("letni",variants,l) -> Variant(Xlist.rev_map variants (merge_letni l)) (*in print_endline (ENIAMtokens.string_of_tokens 0 t); t*)
  | AR("brev",variants,l) -> Variant(Xlist.rev_map variants (fun ets -> Seq(Xlist.map (set_sent_list ets l) (fun t -> Token t))))
  | AR("both-correct",variants,l) -> Variant(Xlist.rev_map variants (fun ets -> Seq(Xlist.map (set_sent_list ets l) (fun t -> Token t))))
  | AR("eniam-correct",variants,l) -> Variant(Xlist.rev_map variants (fun ets -> Seq(Xlist.map (set_sent_list ets l) (fun t -> Token t))))
  | AR("nkjp-correct",variants,l) -> Seq(Xlist.map l (fun n -> set_sent n.nsent {empty_token_env with orth=n.north; token=Lemma(n.nlemma,n.ncat,[transform_nkjp_interp_simple n.ncat n.ninterp])})) (* FIXME: ustalenie beg len next *)
  | t -> failwith ("merge_paragraph: " ^ string_of_atoken t)

let test_annotate name typ channel entries =
  (* if name = "620-3-010001854" then prerr_endline "620-3-010001854 omited" else ( *)
  prerr_endline name;
  Xlist.iter entries (fun (id_div,has_ne,paragraphs) ->
    Xlist.iter paragraphs (fun (paragraph,sentences) ->
      let tokens = flatten_sentences sentences in
      let tokens = simple_allign "" "" [] tokens in
      let paragraph = render_paragraph tokens in
      (*let s = "Trwa trzeci i ostatni etap" in
      let s = "Dyskografię Blackoutu uzupełnia " in
      let s = "W końcu jednak kupiłem " in
      let s = "No i jeszcze te jego strasznie " in
      let s = "I tak wszędzie. Człowiek" in*)
      (*if String.length paragraph >= String.length s && String.sub paragraph 0 (String.length s) = s then*) (
      (* print_endline paragraph; *)
      let tokens = remove_spaces [] tokens in
      let eniam_tokens = ENIAMtokenizer.parse paragraph in
      let eniam_tokens = convert_eniam_tokens [] eniam_tokens in
      (* print_endline "test_annotate 1"; *)
      let eniam_tokens = annotate_variants_par eniam_tokens in
      (* print_endline "test_annotate 2"; *)
      let m = annotate_paragraph name paragraph tokens eniam_tokens in
      (* print_endline "test_annotate 3"; *)
      check_annotation paragraph m;
      let _ = List.rev (Xlist.rev_map m (merge_paragraph name)) in
      ());
      (* print_endline (String.concat "\n" (Xlist.map m string_of_atoken))); *)
      ()))

type cap = Capital | Small | Sign

let classify_cap s =
   match Xunicode.classified_chars_of_utf8_string s with
        Xunicode.Capital _ :: _ -> Capital
      | Xunicode.ForeignCapital _ :: _ -> Capital
      | Xunicode.Small _ :: _ -> Small
      | Xunicode.ForeignSmall _ :: _ -> Small
      | _ -> Sign

let rec get_ntoken = function
    (Disamb(nlemma,ncat,ninterp) : attr) :: _ -> nlemma,ncat,ninterp
  | _ :: l -> get_ntoken l
  | [] -> raise Not_found

let rec disambiguate_capitalics = function
    Token t ->
      (try
        let nlemma,ncat,ninterp = get_ntoken t.attrs in
        let c = match t.token, classify_cap nlemma with
          ENIAMtokenizerTypes.SmallLetter _, Small -> true
        | ENIAMtokenizerTypes.CapLetter _, Capital -> true
        | ENIAMtokenizerTypes.AllSmall _ , Small-> true
        | ENIAMtokenizerTypes.AllCap _, Capital -> true
        (* | ENIAMtokenizerTypes.AllCap _, Small -> true *)
        | ENIAMtokenizerTypes.FirstCap _, Capital -> true
        | ENIAMtokenizerTypes.SomeCap _, Capital -> true
        | ENIAMtokenizerTypes.SomeCap _, Small -> true
        | ENIAMtokenizerTypes.RomanDig _, Capital -> true
        | ENIAMtokenizerTypes.Interp _, _ -> true
        | ENIAMtokenizerTypes.Symbol _, _ -> true
        | ENIAMtokenizerTypes.Dig _, _ -> true
        | ENIAMtokenizerTypes.Other _, _ -> true
        | ENIAMtokenizerTypes.Lemma _, _ -> true
        | ENIAMtokenizerTypes.Proper _, _ -> true
        | ENIAMtokenizerTypes.Compound _, _ -> true
        | ENIAMtokenizerTypes.Tokens _, _ -> true
        | _ -> false in
        Token t, c
        (* let nc = classify_cap nlemma in
        let no = classify_cap t.orth in
        if no = nc then Token t,true else Token t,false *)
      with Not_found -> Token t,true)
  | Seq l ->
      let l,c = Xlist.fold l ([],true) (fun (l,c) t ->
        let t,d = disambiguate_capitalics t in
        t :: l, c && d) in
      Seq(List.rev l), c
  | Variant l ->
      let l2 = Xlist.fold l [] (fun l t ->
        let t,d = disambiguate_capitalics t in
        if d then t :: l else l) in
      if l2 = [] then Variant l,false else Variant l2,true

let annotate name sentences =
  let tokens = flatten_sentences sentences in
  let tokens = simple_allign "" "" [] tokens in
  let paragraph = render_paragraph tokens in
  let tokens = remove_spaces [] tokens in
  let eniam_tokens = ENIAMtokenizer.parse paragraph in
  let eniam_tokens = convert_eniam_tokens [] eniam_tokens in
  let eniam_tokens = annotate_variants_par eniam_tokens in
  let m = annotate_paragraph name paragraph tokens eniam_tokens in
  let m = List.rev (Xlist.rev_map m (merge_paragraph name)) in
  let m = List.rev (Xlist.fold m [] (fun m t ->
    let t,_ = disambiguate_capitalics t in
    t :: m)) in
  paragraph, m

let test_disambiguate_capitalics stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let paragraph,tokens = annotate name sentences in
      Xlist.fold tokens stats (fun stats t ->
        let _,c = disambiguate_capitalics t in
        if c then stats else StringQMap.add stats (Printf.sprintf "%s %s" (ENIAMtokens.string_of_tokens 0 t) paragraph))))


let selection = StringSet.of_list [(*"Rzeczpospolita";"200-4-000014";"040-2-000007";"120-2-900126";"120-2-910000001";"120-2-910000002";"120-4-900005";
"620-3-010001110";"620-3-010001449";"620-3-010001622";"620-3-010001727";
"620-3-010001731";"620-3-010001741";"620-3-010001854";"711-3-010000051";"711-3-010000056";
"711-3-010000079";"720-3-010000217";"720-3-010000335";"720-3-010000341";"forumowisko.pl_18535";"forumowisko.pl_424";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";"";*)
  (* "040-2-000001";"040-2-000007";"040-4-000000103";"120-2-000003";"120-2-000007";"120-2-000009";"120-2-000010";"120-2-900017";"120-2-900041";"120-2-900044";"120-2-900083";
  "120-2-900092";"120-2-900094";"120-2-900123";"120-2-910000011";"120-4-900000001";"120-4-900008";"120-4-900010";"130-3-900001";"130-3-910001";"130-5-000000267";
  "130-5-000000406";"130-5-000000817";"130-5-000001188";"130-5-000001274";"130-5-000001338";"130-5-000001628";"130-5-000001742";"200-1-000011";"200-1-000026";"200-2-000078";
  "200-2-000173";"200-2-000175";"200-4-000000307";"200-4-000000316";"310-2-000007";"320-2-000000094";"320-2-000034";"320-2-000064";"320-3-000226";"330-2-000000030";
  "330-2-000000033";"330-2-000000200";"330-2-000000213";"330-2-000003";"330-2-000013";"620-3-010000057";"620-3-010000838";"620-3-010001103";"620-3-010001107";"620-3-010001108";
  "620-3-010001109";"620-3-010001125";"620-3-010001274";"620-3-010001448";"620-3-010001732";"620-3-010001772";"711-3-010000021";"712-1-900003";"712-1-900004";"720-3-000071";
  "720-3-010000323";"DP1999";"DP2002";"DP2003";"EkspressWieczorny";"forumowisko.pl_20218";"forumowisko.pl_42911";"forumowisko.pl_724";"GazetaGoleniowska";"GazetaTczewska";
  "NIE";"SuperExpress";"TrybunaSlaska"; *)
  (* "120-2-000009";"120-2-000010";"120-2-000012";"120-2-900019";"120-2-900041";"120-2-900044";"120-2-900092";"120-2-900123";"120-2-910000011";"120-4-900000001";"120-4-900001";
  "120-4-900008";"130-3-900001";"130-5-000000267";"130-5-000000817";"130-5-000001188";"130-5-000001274";"130-5-000001628";"130-5-000001635";"130-5-000001742";"200-1-000011";
  "200-2-000078";"200-2-000181";"200-4-000000314";"200-4-000026";"200-4-000059";"310-2-000007";"320-2-000000087";"320-2-000000094";"320-2-000034";"330-2-000013";"620-3-010000057";
  "620-3-010000099";"620-3-010000838";"620-3-010000839";"620-3-010001729";"620-3-010001743";"620-3-010001853";"620-3-010001873";"620-3-010001895";"711-3-010000021";"720-3-000071";
  "720-3-010000323";"720-3-010000337";"DP2000";"EkspressWieczorny";"forumowisko.pl_12517";"forumowisko.pl_20218";"forumowisko.pl_42911";"GazetaTczewska";"SuperExpress" *)
  (* "120-2-900092";"120-4-900000001";"120-4-900008";"130-3-900001";"200-2-000078";"200-4-000059";"330-2-000013";"720-3-000071";"720-3-010000337";"EkspressWieczorny" *)
  (* "110-4-000000102";"120-2-000006";"120-2-900032";"120-2-900035";"130-3-900005";"130-3-910001";
  "130-5-000000507";"130-5-000000765";"130-5-000001156";"200-2-000191";"330-2-000000030";
  "620-3-010000835";"620-3-010001772";"DP1999";"GazetaGoleniowska";"GazetaMalborska";"KOT";
  "KurierKwidzynski";"NIE";"Rzeczpospolita";"TrybunaSlaska" *)
  (* "110-4-000000102";"120-2-000006";"120-2-900032";"130-5-000000507";"130-5-000001156";
  "620-3-010000835";"GazetaGoleniowska";"KurierKwidzynski";"NIE";"Rzeczpospolita"; *)
  (*"110-4-000000102";"KurierKwidzynski";*)(*"620-3-010001496;"*)(*"130-5-000001341";*)(*"620-3-010001854"*)(*"620-3-010001106"*)
  (* "310-2-000006" *)
  (* "forumowisko.pl_4644" *)
  "NIE";"Rzeczpospolita";"NeckaCzlowiek";"712-1-900001";
]

let _ =
  (* ENIAMtokenizer.initialize (); *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] StringQMap.empty (fun stats (name,typ,channel,entries) ->
    count_variants stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    count_variants stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate_segmentation stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate_segmentation stats name typ channel entries) in *)
  (* ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] () (fun () (name,typ,channel,entries) ->
    test_annotate name typ channel entries); *)
  (* ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path () (fun () (name,typ,channel,entries) ->
    test_annotate name typ channel entries); *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] StringQMap.empty (fun stats (name,typ,channel,entries) ->
    test_disambiguate_capitalics stats name typ channel entries) in *)
  (* let stats = StringQMap.fold stats [] (fun stats k v -> (v,k) :: stats) in
  Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%d\t%s\n" v k); *)
  (* ignore(Sys.command "mpg123 \"../../Inne/gong/gong_00m_30s.mp3\""); *)
  ()

(*let eniam_correct = StringSet.of_list (File.load_lines "data/eniam-correct.tab")
let nkjp_correct = StringSet.of_list (File.load_lines "data/nkjp-correct.tab")

let space = {empty_token_env with orth=" "; token=Symbol " "}
let query_beg = {empty_token_env with token=Interp "<query>"}
let query_end = {empty_token_env with token=Interp "</query>"}
let sencence_beg = {empty_token_env with token=Interp "<sentence>"}
let sencence_end = {empty_token_env with token=Interp "</sentence>"}
let clause_beg = {empty_token_env with token=Interp "<clause>"}
let clause_end = {empty_token_env with token=Interp "</clause>"}



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

let rec set_lengths n rev = function
    t :: l ->
       let len =
         if t.token = Interp "<query>" || t.token = Interp "</query>" then factor else
         Xlist.size (Xunicode.utf8_chars_of_utf8_string t.orth) * factor in
       set_lengths (n+len) ({t with beg=n; len=len; next=n+len} :: rev) l
  | [] -> List.rev rev

(* FIXME: poprawić interpretacje przecinka i innych znaków interpunkcyjnych *)
let rec set_special_tokens_lengths rev = function
    ({token=Interp "<sentence>"} as sent) :: ({token=Interp "<clause>"} as cl) :: t :: l ->
       let sent = {sent with len=1; next=sent.beg+1} in
       let cl = {cl with beg=sent.next; len=1; next=sent.next+1} in
       let t = {t with beg=t.beg+2; len=t.len-2} in
       set_special_tokens_lengths (Token t :: Token cl :: Token sent :: rev) l
  | ({orth="."; token=Lemma(".","interp",[[]])} as dot) :: ({token=Interp "</clause>"} as cl) :: {token=Interp "</sentence>"} :: l ->
       let cl = {cl with beg=dot.beg; len=20; next=dot.beg+20} in
       let dot = {dot with beg=cl.next; len=80; token= Interp "</sentence>"} in
       set_special_tokens_lengths (Token dot :: Token cl :: rev) l
  | t :: l -> set_special_tokens_lengths (Token t :: rev) l
  | [] -> List.rev rev

let render_paragraph tokens =
  String.concat "" (List.rev (Xlist.rev_map tokens (fun t -> t.orth)))

let rec get_next = function
    Token t -> t.next
  | Seq [] -> failwith "get_next"
  | Seq l -> get_next (List.hd (List.rev l))
  | Variant [] -> failwith "get_next"
  | Variant l -> get_next (List.hd l)

let rec get_beg = function
    Token t -> t.beg
  | Seq [] -> failwith "get_beg"
  | Seq l -> get_beg (List.hd l)
  | Variant [] -> failwith "get_beg"
  | Variant l -> get_beg (List.hd l)

let make_seq  = function
    [] -> failwith "make_seq"
  | [t] -> t
  | l -> Seq l

let rec match_token_sequence erev nrev rev = function
    et :: ets, nt :: nts ->
      let enext = get_next et in
      let nnext = get_next nt in
      if enext = nnext then
        match_token_sequence [] [] ((List.rev (et :: erev), List.rev (nt :: nrev)) :: rev) (ets,nts)
      else if enext < nnext then
        match_token_sequence (et :: erev) nrev rev (ets, nt :: nts)
      else match_token_sequence erev (nt :: nrev) rev (et :: ets, nts)
  | [],[] -> Xlist.fold rev [] (fun l (et,nt) -> (make_seq et, make_seq nt) :: l)
  | ets,nts ->
      let s = Printf.sprintf "%s" (ENIAMtokens.string_of_tokens 0 (Seq ets)) in
      let t = Printf.sprintf "%s" (ENIAMtokens.string_of_tokens 0 (Seq nts)) in
      (*failwith*)print_endline (Printf.sprintf "match_token_sequence: %s\n\n%s\n" s t); []

let rec compare_tokens = function
    Token et, Token nt ->
       et.orth = nt.orth && et.beg = nt.beg && et.len = nt.len && et.next = nt.next
  | et,Variant l ->
       Xlist.fold l true (fun b nt ->
         compare_tokens (et,nt) && b)
  | Variant l,nt ->
       Xlist.fold l false (fun b et ->
         compare_tokens (et,nt) || b)
  | Seq[et], nt -> compare_tokens (et,nt)
  | et, Seq[nt] -> compare_tokens (et,nt)
  | Seq(et::ets),Seq(nt::nts) -> if compare_tokens (et,nt) then compare_tokens (Seq ets,Seq nts) else false
  | _ -> false

let rec shift_token_rec beg = function
    Token t -> Token{t with beg=t.beg-beg; next=t.next-beg}
  | Seq l -> Seq(Xlist.map l (shift_token_rec beg))
  | Variant l -> Variant(Xlist.map l (shift_token_rec beg))

let shift_token t =
  let beg = get_beg t in
  shift_token_rec beg t

let string_of_tokens_complete eniam_token nkjp_token =
  let s = ENIAMtokens.string_of_tokens 0 (shift_token eniam_token) in
  let t = ENIAMtokens.string_of_tokens 0 (shift_token nkjp_token) in
  s ^ "\n" ^ t

let rec string_of_tokens_simple = function
    Token t -> if t.orth = "" then ENIAMtokens.get_orth t.token(*failwith "string_of_tokens_simple"*) else t.orth
  | Seq l -> String.concat " " (Xlist.map l string_of_tokens_simple)
  | Variant l ->
      (match StringSet.to_list (StringSet.of_list (Xlist.map l string_of_tokens_simple)) with
        [] -> failwith "string_of_tokens_simple"
      | [s] -> s
      | l -> "[" ^ String.concat "; " l ^ "]")

let string_of_tokens_simple eniam_token nkjp_token =
  try
    string_of_tokens_simple eniam_token ^ " <---> " ^
    string_of_tokens_simple nkjp_token
  with _ -> "EMPTY ORTH"

let validate addition_fun stats name typ channel entries =
  print_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    (* if id_div = 3 then *)
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      (* Printf.printf "%d\t%s\n" id_div paragraph; *)
      let tokens = flatten_sentences sentences in
      let tokens = allign "" "" [] (set_beg_as_zero tokens) in
      let paragraph = render_paragraph tokens in
      (* Printf.printf "rend:\t%s\n" paragraph; *)
      let tokens = set_lengths 0 [] tokens in
      let tokens = set_special_tokens_lengths [] tokens in
      let tokens = ENIAMpatterns.remove_spaces [] tokens in
      let eniam_tokens = ENIAMtokenizer.parse paragraph in
      (* Printf.printf "eniam_tokens: %s\n" (ENIAMtokens.string_of_tokens 0 (Seq eniam_tokens));
      Printf.printf "tokens: %s\n" (ENIAMtokens.string_of_tokens 0 (Seq tokens)); *)
      let l = match_token_sequence [] [] [] (eniam_tokens,tokens) in
      Xlist.fold l stats (fun stats (eniam_token,nkjp_token) ->
        if compare_tokens (eniam_token,nkjp_token) then stats else
        if StringSet.mem eniam_correct (string_of_tokens_simple eniam_token nkjp_token) then stats else
        if StringSet.mem nkjp_correct (string_of_tokens_simple eniam_token nkjp_token) then stats else
          StringQMap.add stats (addition_fun eniam_token nkjp_token)
        (*
          let s = Printf.sprintf "%s" (ENIAMtokens.string_of_tokens 0 (shift_token eniam_token)) in
          let t = Printf.sprintf "%s" (ENIAMtokens.string_of_tokens 0 (shift_token nkjp_token)) in
          (* Printf.printf "%s\n%s\n\n%!" s t; *)
          StringQMap.add stats (s ^ "\n" ^ t)*))) (*else stats*))

let validate_segmentation addition_fun stats name typ channel entries =
  print_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    (* if id_div = 3 then *)
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      (* Printf.printf "%d\t%s\n" id_div paragraph; *)
      let tokens = flatten_sentences sentences in
      let tokens = allign "" "" [] (set_beg_as_zero tokens) in
      let paragraph = render_paragraph tokens in
      (* Printf.printf "rend:\t%s\n" paragraph; *)
      let tokens = set_lengths 0 [] tokens in
      let tokens = set_special_tokens_lengths [] tokens in
      let tokens = ENIAMpatterns.remove_spaces [] tokens in
      let eniam_tokens = ENIAMtokenizer.parse paragraph in
      (* Printf.printf "eniam_tokens: %s\n" (ENIAMtokens.string_of_tokens 0 (Seq eniam_tokens));
      Printf.printf "tokens: %s\n" (ENIAMtokens.string_of_tokens 0 (Seq tokens)); *)
      let l = match_token_orth_sequence [] [] [] (eniam_tokens,tokens) in
      Xlist.fold l stats (fun stats (eniam_token,nkjp_token) ->
        if compare_tokens (eniam_token,nkjp_token) then stats else
        if StringSet.mem eniam_correct (string_of_tokens_simple eniam_token nkjp_token) then stats else
        if StringSet.mem nkjp_correct (string_of_tokens_simple eniam_token nkjp_token) then stats else
          StringQMap.add stats (addition_fun eniam_token nkjp_token)
        (*
          let s = Printf.sprintf "%s" (ENIAMtokens.string_of_tokens 0 (shift_token eniam_token)) in
          let t = Printf.sprintf "%s" (ENIAMtokens.string_of_tokens 0 (shift_token nkjp_token)) in
          (* Printf.printf "%s\n%s\n\n%!" s t; *)
          StringQMap.add stats (s ^ "\n" ^ t)*))) (*else stats*))
*)
  (* let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate string_of_tokens_complete stats name typ channel entries) in
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate string_of_tokens_complete stats name typ channel entries) in *)
  let stats = StringQMap.fold stats [] (fun stats k v -> (v,k) :: stats) in
  Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%d\n%s\n" v k); *)
  (* let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate string_of_tokens_simple stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate string_of_tokens_simple stats name typ channel entries) in *)
