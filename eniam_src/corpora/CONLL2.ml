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
open ENIAMsubsyntaxTypes
open ENIAMtokenizerTypes

exception Comment_line
exception Empty_line
exception Empty_sentence
exception Sent_id of string
exception Raw_text of string
exception Orig of string
exception Interval_id of int

let load_token beg compound in_channel =
  let n_token id orth beg lemma interp sl sem sp =
    let sp = match sp with
        "_" -> if compound > 0 then 0 else 100
      | "SpaceAfter=No" -> 0
      | _ -> failwith ("load_token sp: " ^ sp) in
    let sem = match sem with
        "_" -> ""
      | _ -> sem in
    let len = (Xlist.size (Xunicode.utf8_chars_of_utf8_string orth)) * 100 in
    let next = beg+len+sp in
    let id = try int_of_string id with _ ->
      let len = match Xstring.split "-" id with
          [a;b] -> (try int_of_string b - int_of_string a with _ -> failwith "load_token: interval id")
        | _ -> failwith "load_token: interval id" in
      raise (Interval_id len) in
    let pos,tags = match ENIAMtagset.parse interp with [x] -> x | _ -> failwith "n_token" in
    {empty_token_env with orth = orth; beg=beg; len=len; next=next;
      token = Lemma(lemma,pos,[tags])}, next, id, sl, sem in
  let line = input_line in_channel in
  if line = ""
   then raise Empty_line
   else if line.[0] = '#'
     then
       if Xstring.check_prefix "# sent_id = " line then
         raise (Sent_id(Xstring.cut_prefix "# sent_id = " line)) else
       if Xstring.check_prefix "# text = " line then
         raise (Raw_text(Xstring.cut_prefix "# text = " line)) else
       if Xstring.check_prefix "# orig_file_sentence = " line then
         raise (Orig(Xstring.cut_prefix "# orig_file_sentence = " line)) else
       raise Comment_line
     else
       match Xstring.split "\t" line with
         [id; orth; lemma; ucat; interp; uinterp; super; label; "_"; sp] ->
          let super = if super = "_" then 0 else try int_of_string super with _ -> failwith ("load_token super: " ^ super) in
          n_token id orth beg lemma interp [super,label] "_" sp
       | [id; orth; lemma; ucat; interp; uinterp; super; label; sl; sp; sem] ->
          let sl = match sl with
              "_:_" -> []
            | _ -> Xlist.map (Xstring.split "|" sl) (fun s ->
              match Xstring.split ":" s with
                super :: l -> (try int_of_string super, String.concat ":" l with _ -> failwith ("load_token sl: " ^ sl))
              | _ -> failwith ("load_token sl: " ^ sl)) in
          n_token id orth beg lemma interp sl sem sp
     | _ -> failwith ("load_token: " ^ line)

let substract_next tokens = function
    ((id,_,_) :: _) as rev_paths ->
      let t = ExtArray.get tokens id in
      ExtArray.set tokens id {t with next=t.next-100};
      rev_paths
  | _ -> failwith "substract_next"

let load_sentence in_channel =
  let tokens = ExtArray.make 100 empty_token_env in
  let _ = ExtArray.add tokens {empty_token_env with token = Interp "<conll_root>"} in
  let rec pom rev_paths next compound sent_id text orig =
    try
      let token, next, conll_id, sl, sem = load_token next compound in_channel in
      let id_a = ExtArray.add tokens token in
      if id_a <> conll_id then failwith "load_sentence: different ids" else
      pom ((id_a,sl,sem) :: rev_paths) next (max 0 (compound-1)) sent_id text orig
    with
        Sent_id sent_id -> pom rev_paths next compound sent_id text orig
      | Raw_text text -> pom rev_paths next compound sent_id text orig
      | Orig orig -> pom rev_paths next compound sent_id text orig
      | Comment_line -> failwith "load_sentence: Comment_line"
      | Interval_id len -> (*print_endline line;*) pom rev_paths next len sent_id text orig
      | Empty_line -> substract_next tokens rev_paths, sent_id, text, orig
      | End_of_file -> if rev_paths = []
          then raise End_of_file
          else substract_next tokens rev_paths, sent_id, text, orig in
  let rev_paths, sent_id, text, orig  = pom [] 100 0 "" "" "" in
  {id = sent_id; beg = -1; len = -1; next = -1; file_prefix = ""; sentence = DepSentence[Array.of_list ((0,[],"") :: List.rev rev_paths)]}, text, orig, tokens
(*  {s_id = id; s_text = ""; s_paths = (List.rev rev_paths)} *)

let load_corpus in_channel =
  let rec pom res =
    try
      let conll_sentence, text, orig, tokens = load_sentence in_channel in
      pom ((conll_sentence, text, orig, tokens) :: res)
    with End_of_file -> res
    (*| e -> prerr_endline (Printexc.to_string e); res*) in
  List.rev @@ pom []

let substring a beg len =
  String.concat "" (List.rev (Int.fold beg (beg+len-1) [] (fun l i ->
    a.(i) :: l)))

let verify_lengths corpus =
  Xlist.iter corpus (fun (conll_sentence, text, orig, tokens) ->
    let text = Array.of_list (Xunicode.utf8_chars_of_utf8_string text) in
    Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
      let t = ExtArray.get tokens i in
      let beg = t.beg/100 - 1 in
      let len = t.len/100 in
      let next = t.next/100 - 1 in
      let s = substring text beg len in
      if s <> t.orth then Printf.printf "%s: %s %s\n" conll_sentence.id s t.orth;
      if beg + len = next then () else
      if beg + len + 1 = next then
        if substring text (next-1) 1 = " " then () else Printf.printf "%s: space problem\n" conll_sentence.id else
      Printf.printf "%s: next problem\n" conll_sentence.id))

let get_tagset corpus =
  Xlist.fold corpus StringQMap.empty (fun qmap (conll_sentence, text, orig, tokens) ->
    Int.fold 1 (ExtArray.size tokens - 1) qmap (fun qmap i ->
      let t = ExtArray.get tokens i in
      match t.token with
        Lemma(lemma,cat,interp) -> StringQMap.add qmap (cat ^ ":" ^ ENIAMtagset.render interp)
      | _ -> failwith "get_tagset"))

let numbers = StringSet.of_list ["sg";"pl"]
let cases = StringSet.of_list ["nom";"gen";"dat";"acc";"inst";"loc";"voc"]
let genders = StringSet.of_list ["m1";"m2";"m3";"n";"f"]
let degrees = StringSet.of_list ["pos";"com";"sup"]

let convert_n n =
  if StringSet.mem numbers n then n else failwith ("convert_n: " ^ n)

let convert_c c =
  if StringSet.mem cases c then c else failwith ("convert_c: " ^ c)

let convert_g = function
    "n1" -> "n"
  | "n2" -> "n"
  | "p1" -> "m1"
  | "p2" -> "n"
  | g -> if StringSet.mem genders g then g else failwith ("convert_g: " ^ g)

let convert_d d =
  if StringSet.mem degrees d then d else failwith ("convert_d: " ^ d)

let convert_tagset_token id = function
    Lemma(lemma,"adj",[[[n];[c];[g];[d]]]) -> Lemma(lemma,"adj",[[[convert_n n];[convert_c c];[convert_g g];[convert_d d]]])
  | Lemma(lemma,"adja",[[]]) as t -> t
  | Lemma(lemma,"adjc",[[]]) as t -> t
  | Lemma(lemma,"adjp",[[]]) as t -> t
  | Lemma(lemma,"adv",[[]]) -> Lemma(lemma,"adv",[[["pos"]]])
  | Lemma(lemma,"adv",[[[d]]]) -> Lemma(lemma,"adv",[[[convert_d d]]])
  | Lemma(lemma,"aglt",_) as t -> t
  | Lemma(lemma,"bedzie",_) as t -> t
  | Lemma(lemma,"brev",_) as t -> t
  | Lemma(lemma,"burk",[[]]) as t -> t
  | Lemma(lemma,"burk",[_]) -> Lemma(lemma,"burk",[[]])
  | Lemma(lemma,"comp",[[]]) as t -> t
  | Lemma(lemma,"conj",[[]]) as t -> t
  | Lemma(lemma,"depr",[[["pl"];["nom"];["m2"]]]) as t -> t
  | Lemma(lemma,"depr",[[["pl"];["voc"];["m2"]]]) as t -> t
  | Lemma(lemma,"depr",[[[n];[c];["m1"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m1"]]])
  | Lemma(lemma,"depr",[[[n];[c];["m2"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m2"]]])
  | Lemma(lemma,"depr",[[[n];[c];["m3"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m3"]]])
  | Lemma(lemma,"depr",[[[n];[c];["f"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["f"]]])
  | Lemma(lemma,"dig",[[]]) as t -> t
  | Lemma(lemma,"emo",[[]]) as t -> t
  | Lemma(lemma,"fin",_) as t -> t
  | Lemma(lemma,"ger",[[[n];[c];[g];[a];[neg]]]) -> Lemma(lemma,"ger",[[[convert_n n];[convert_c c];[convert_g g];[a];[neg]]])
  | Lemma(lemma,"imps",_) as t -> t
  | Lemma(lemma,"impt",_) as t -> t
  | Lemma(lemma,"inf",_) as t -> t
  | Lemma(lemma,"interj",[[]]) as t -> t
  | Lemma(lemma,"interp",[[]]) as t -> t
  | Lemma(lemma,"num",[[[n];[c];["m1"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m1"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["m2"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m2"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["m3"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m3"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["f"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["f"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["n"];[acm];["col"]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["n"];[acm];["col"]]])
  | Lemma(lemma,"num",[[[n];[c];["n"];[acm];["ncol"]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["n"];[acm];["ncol"]]])
  | Lemma(lemma,"num",[[[n];[c];["n1"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["n"];[acm];["col"]]])
  | Lemma(lemma,"num",[[[n];[c];["n2"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["n"];[acm];["ncol"]]])
  | Lemma(lemma,"num",[[[n];[c];["p1"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m1"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["p2"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["n"];[acm];["col";"ncol"]]])
  | Lemma(lemma,"num",[[[n];[c];["m1"];[acm];[_]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m1"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["m2"];[acm];[_]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m2"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["m3"];[acm];[_]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m3"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["f"];[acm];[_]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["f"];[acm]]])
  | Lemma(lemma,"num",[[[n];[c];["n"];[acm]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["n"];[acm];["col";"ncol"]]])
  | Lemma(lemma,"num",[[[n];[c];["m3"]]]) -> Lemma(lemma,"num",[[[convert_n n];[convert_c c];["m3"];["congr";"rec"]]])
  | Lemma(lemma,"pact",[[[n];[c];[g];[a];[neg]]]) -> Lemma(lemma,"pact",[[[convert_n n];[convert_c c];[convert_g g];[a];[neg]]])
  | Lemma(lemma,"pant",_) as t -> t
  | Lemma(lemma,"pcon",_) as t -> t
  | Lemma(lemma,"ppas",[[[n];[c];[g];[a];[neg]]]) -> Lemma(lemma,"ppas",[[[convert_n n];[convert_c c];[convert_g g];[a];[neg]]])
  | Lemma(lemma,"ppron12",[[[n];[c];[g];[p]]]) -> Lemma(lemma,"ppron12",[[[convert_n n];[convert_c c];[convert_g g];[p]]])
  | Lemma(lemma,"ppron12",[[[n];[c];[g];[p];[akc]]]) -> Lemma(lemma,"ppron12",[[[convert_n n];[convert_c c];[convert_g g];[p];[akc]]])
  | Lemma(lemma,"ppron3",[[[n];[c];[g];[p];[akc];[praep]]]) -> Lemma(lemma,"ppron3",[[[convert_n n];[convert_c c];[convert_g g];[p];[akc];[praep]]])
  | Lemma(lemma,"praet",[[[n];[g];[a]]]) -> Lemma(lemma,"praet",[[[convert_n n];[convert_g g];[a]]])
  | Lemma(lemma,"praet",[[[n];[g];[a];[agl]]]) -> Lemma(lemma,"praet",[[[convert_n n];[convert_g g];[a];[agl]]])
  | Lemma(lemma,"pred",[[]]) as t -> t
  | Lemma(lemma,"prep",_) as t -> t
  | Lemma(lemma,"qub",_) as t -> t
  | Lemma(lemma,"romandig",[[]]) as t -> t
  | Lemma(lemma,"siebie",_) as t -> t
  | Lemma(lemma,"subst",[[[n];[c];["m1"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m1"]]])
  | Lemma(lemma,"subst",[[[n];[c];["m2"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m2"]]])
  | Lemma(lemma,"subst",[[[n];[c];["m3"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m3"]]])
  | Lemma(lemma,"subst",[[[n];[c];["f"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["f"]]])
  | Lemma(lemma,"subst",[[[n];[c];["n"];["col"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["col"]]])
  | Lemma(lemma,"subst",[[[n];[c];["n"];["ncol"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["ncol"]]])
  | Lemma(lemma,"subst",[[[n];[c];["m1"];["pt"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m1"];["pt"]]])
  | Lemma(lemma,"subst",[[[n];[c];["n"];["pt"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["pt"]]])
  | Lemma(lemma,"subst",[[[n];[c];["n1"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["col"]]])
  | Lemma(lemma,"subst",[[[n];[c];["n2"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["ncol"]]])
  | Lemma(lemma,"subst",[[[n];[c];["p1"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m1"];["pt"]]])
  | Lemma(lemma,"subst",[[[n];[c];["p2"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["pt"]]])
  | Lemma(lemma,"subst",[[[n];[c];["m3"];[_]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["m3"]]])
  | Lemma(lemma,"subst",[[[n];[c];["n"]]]) -> Lemma(lemma,"subst",[[[convert_n n];[convert_c c];["n"];["ncol"]]])
  | Lemma(lemma,"winien",[[[n];[g];[a]]]) -> Lemma(lemma,"winien",[[[convert_n n];[convert_g g];[a]]])
  | Lemma("Crimeboys" as lemma,"ign",[[]]) -> Lemma(lemma,"subst",[[["pl"];["nom"];["m1"]]])
  | Lemma("109P4" as lemma,"ign",[[]]) -> Lemma(lemma,"subst",[[["sg"];["nom"];["m2"]]])
  | Lemma("1a." as lemma,"ign",[[]]) -> Lemma(lemma,"list-item",[[]])
  | Lemma("orfano" as lemma,"ign",[[]]) -> Lemma(lemma,"xxx",[[]])
  | Lemma("650-91-58" as lemma,"ign",[[]]) -> Lemma(lemma,"phone-number",[[]])
  | Lemma("654-66-91" as lemma,"ign",[[]]) -> Lemma(lemma,"phone-number",[[]])
  | Lemma("U2" as lemma,"ign",[[]]) -> Lemma(lemma,"subst",[[["sg"];["gen"];["m1"]]])
  | Lemma("Uudenkaupungin" as lemma,"ign",[[]]) -> Lemma(lemma,"subst",[[["sg"];["gen"];["m1"]]])
  | Lemma("kaupunki" as lemma,"ign",[[]]) -> Lemma(lemma,"subst",[[["sg"];["gen"];["m1"]]])
  | Lemma("AKP" as lemma,"ign",[[]]) -> Lemma(lemma,"subst",[[[""];[""];[""]]])
  | Lemma("Beginning" as lemma,"ign",[[]]) -> Lemma(lemma,"xxx",[[]])
  | Lemma("with" as lemma,"ign",[[]]) -> Lemma(lemma,"xxx",[[]])
  | Lemma("my" as lemma,"ign",[[]]) -> Lemma(lemma,"xxx",[[]])
  | Lemma("streets" as lemma,"ign",[[]]) -> Lemma(lemma,"xxx",[[]])
  | t -> print_endline ("convert_tagset_token: " ^ id ^ " " ^ ENIAMtokens.string_of_token t);t

let convert_tagset corpus =
  Xlist.iter corpus (fun (conll_sentence, text, orig, tokens) ->
    Int.iter 1 (ExtArray.size tokens - 1) (fun i ->
      let t = ExtArray.get tokens i in
      let token = convert_tagset_token conll_sentence.id t.token in
      ExtArray.set tokens i {t with token=token}));
  corpus

(*let string_of_depencency = function
    (* Lemma(lemma1,cat1,interp1),"punct",Lemma(lemma2,"interp",_) -> cat1 ^ " -> punct -> " ^ lemma2 ^ ":interp"
  | Interp "<conll_root>","root",Lemma(lemma2,cat2,_) -> "<conll_root> -> root -> " ^ cat2 *)
  | _,"nsubj",_ -> "nsubj"
  | _,"amod",_ -> "amod"
  | _,"root",_ -> "root"
  | _,"punct",_ -> "punct"
  | _,"advmod",_ -> "advmod"
  | _,"expl:impers",_ -> "expl:impers"
  | _,"mark",_ -> "mark" (* ??? *)
  | _,"cc",_ -> "cc"
  | _,"conj",_ -> "conj"
  | _,"compound:aglt",_ -> "compound:aglt"
  | _,"case",_ -> "case"
  | _,"advcl",_ -> "advcl"
  | _,"obj",_ -> "obj"
  | _,"iobj",_ -> "iobj"
  | _,"obl",_ -> "obl"
  | _,"obl:arg",_ -> "obl:arg"
  | _,"appos",_ -> "appos"
  | _,"xcomp",_ -> "xcomp"
  | _,"flat",_ -> "flat"
  | _,"fixed",_ -> "fixed"
  | _,"nmod",_ -> "nmod"
  | _,"nmod:arg",_ -> "nmod:arg"
  | _,"nummod",_ -> "nummod"
  | _,"cop",_ -> "cop"
  | _,"det",_ -> "det"
  | _,"nsubj:pass",_ -> "nsubj:pass"
  | _,"aux",_ -> "aux"
  | _,"aux:pass",_ -> "aux:pass"
  | _,"compound:cnd",_ -> "compound:cnd"
  | _,"parataxis",_ -> "parataxis"
  | _,"ccomp",_ -> "ccomp"
  | _,"acl:relcl",_ -> "acl:relcl"
  | _,"discourse:comment",_ -> "discourse:comment"
  | _,"list",_ -> "list"
  | _,"ccomp:obj",_ -> "ccomp:obj"
  | _,"vocative",_ -> "vocative"
  | _,"csubj",_ -> "csubj"
  | _,"advmod:arg",_ -> "advmod:arg"
  | _,"compound:imp",_ -> "compound:imp"
  | _,"obl:comp",_ -> "obl:comp"
  | _,"cc:preconj",_ -> "cc:preconj"
  | _,"discourse:intj",_ -> "discourse:intj"
  | _,"acl:attrib",_ -> "acl:attrib"
  | _,"nmod:title",_ -> "nmod:title"
  | _,"obl:agent",_ -> "obl:agent"
  | _,"orphan",_ -> "orphan"
  | _,"nmod:subj",_ -> "nmod:subj"
  | _,"obl:pass",_ -> "obl:pass"
  | _,"discourse:emo",_ -> "discourse:emo"
  | (Lemma(lemma1,"subst",[[_] :: [c1] :: _]) as s),"case",(Lemma(lemma2,"prep",[[c2] :: _]) as t) ->
      if c1 = c2 then "subst" ^ " -> case -> " ^ "prep" else ENIAMtokens.string_of_token s ^ " -> " ^ "case" ^ " -> " ^ ENIAMtokens.string_of_token t
  | Lemma(lemma1,cat1,interp1),"case",Lemma(lemma2,"adv",interp2) -> cat1 ^ ":" ^ ENIAMtagset.render interp1 ^ " -> case -> " ^ lemma2 ^ ":" ^ "adv" ^ ":" ^ ENIAMtagset.render interp2
  | Lemma(lemma1,cat1,interp1),label,Lemma(lemma2,cat2,interp2) ->
      cat1 ^ ":" ^ ENIAMtagset.render interp1 ^ " -> " ^ label ^ " -> " ^ cat2 ^ ":" ^ ENIAMtagset.render interp2
  | s,label,t -> ENIAMtokens.string_of_token s ^ " -> " ^ label ^ " -> " ^ ENIAMtokens.string_of_token t

let list_dependencies corpus =
  Xlist.fold corpus StringQMap.empty (fun qmap (conll_sentence, text, orig, tokens) ->
    let a = match conll_sentence.sentence with
        DepSentence[a] -> a
      | _ -> failwith "list_dependencies" in
    Int.fold 1 (Array.length a - 1) qmap (fun qmap i ->
      let id,sl,sem = a.(i) in
      Xlist.fold sl qmap (fun qmap (super,label) ->
        let super_id,_,_ = a.(super) in
        let t = ExtArray.get tokens id in
        let s = ExtArray.get tokens super_id in
        StringQMap.add qmap (string_of_depencency (s.token,label,t.token)))))*)

type dep =
  {id: int; tid: int; lemma: string; cat: string; interp: string list list list;
   label: string; sem: string; sons: tree list; is_shared: bool}

and tree =
    Dep of dep
  | Cluster of (string * string list list list) * dep * tree list (* nazwa frazy * komponenty * podrzędniki *)
  | Coordination of string * string * tree list * tree list

let empty_dep = {id=(-1); tid=(-1); lemma=""; cat=""; interp=[]; label=""; sem=""; sons=[]; is_shared=false}

let string_of_sem sem =
  if sem = "" then "" else "[" ^ sem ^ "]"

let string_of_lci d =
  let interp = ENIAMtagset.render d.interp in
  if interp = "" then Printf.sprintf "%s,%s" d.lemma d.cat
  else Printf.sprintf "%s,%s:%s" d.lemma d.cat interp

let string_of_phrase (phrase,interp) =
  let interp = ENIAMtagset.render interp in
  if interp = "" then phrase
  else Printf.sprintf "%s:%s" phrase interp

let rec string_of_tree spaces = function
    Dep d ->
      if d.sons = [] then Printf.sprintf "%s%sDep(%d,%s,%s%s)" spaces (if d.is_shared then "Shared" else "") d.id (string_of_lci d) d.label (string_of_sem d.sem)
      else Printf.sprintf "%s%sDep(%d,%s,%s%s,[\n%s])" spaces (if d.is_shared then "Shared" else "") d.id (string_of_lci d) d.label (string_of_sem d.sem)
        (String.concat "\n" (Xlist.map d.sons (string_of_tree ("  " ^ spaces))))
  | Cluster((phrase,interp),d,sons) ->
      let dsons = if d.sons = [] then "" else
        ",{\n" ^ String.concat "\n" (Xlist.map d.sons (string_of_tree ("  " ^ spaces))) ^ "}" in
      let sons = if sons = [] then "" else
        ",[\n" ^ String.concat "\n" (Xlist.map sons (string_of_tree ("  " ^ spaces))) ^ "]" in
      Printf.sprintf "%s%sCluster(%d,%s,%s,%s%s%s%s)" spaces (if d.is_shared then "Shared" else "")
        d.id (string_of_phrase (phrase,interp)) (string_of_lci d) d.label (string_of_sem d.sem) dsons sons
  (* | PairDep(d,d2) ->
      if d.sons = [] then Printf.sprintf "%s%sPairDep(%d,%s,%s%s,%s)" spaces (if d.is_shared then "Shared" else "") d.id (string_of_lci d) d.label (string_of_sem d.sem) (string_of_lci d2)
      else Printf.sprintf "%s%sPairDep(%d,%s,%s%s,%s,[\n%s])" spaces (if d.is_shared then "Shared" else "") d.id (string_of_lci d) d.label (string_of_sem d.sem) (string_of_lci d2)
        (String.concat "\n" (Xlist.map d.sons (string_of_tree ("  " ^ spaces)))) *)
  | Coordination(label,sem,sons,[]) -> Printf.sprintf "%sCoordination(%s%s,[\n%s])" spaces label (string_of_sem sem)
      (String.concat "\n" (Xlist.map sons (string_of_tree ("  " ^ spaces))))
  | Coordination(label,sem,sons,coords) -> Printf.sprintf "%sCoordination(%s%s,[\n%s],[\n%s])" spaces label (string_of_sem sem)
      (String.concat "\n" (Xlist.map sons (string_of_tree ("  " ^ spaces))))
      (String.concat "\n" (Xlist.map coords (string_of_tree ("  " ^ spaces))))

let rec get_tree_node_id = function
    Dep d -> d.id
  | Coordination(label,sem,sons,coord) -> get_tree_node_id (List.hd sons)
  | _ -> failwith "get_tree_node_id"

let sort_dependents l =
  Xlist.sort l (fun x y -> compare (get_tree_node_id x) (get_tree_node_id y))

let rec make_tree_rec tokens id tid label b sem sons =
  let l = try IntMap.find sons id with Not_found -> [] in
  let l = Xlist.fold l [] (fun l (id,tid,label,b,sem) ->
    make_tree_rec tokens id tid label b sem sons :: l) in
  let lemma,cat,interp = match (ExtArray.get tokens tid).token with
      Lemma(lemma,cat,interp) -> lemma,cat,interp
    | Interp s -> s,"interp",[[]]
    | _ -> failwith "make_tree_rec" in
  Dep{id=id; tid=tid; lemma=lemma; cat=cat; interp=interp; label=label; sem=sem; sons=l; is_shared=b}

let clean_coord_deps = function
    [] -> []
  | [i,s] -> [i,s]
  | [i1,"conj";i2,s2] -> [i1,"conj"]
  | [i1,s1;i2,"conj"] -> [i2,"conj"]
  | (i,s) :: l ->
      if Xlist.fold l true (fun b (_,t) -> if t = s then b else false) then (i,s) :: l
      else ((*print_endline ("clean_coord_deps: " ^ (String.concat " " (Xlist.map ((i,s) :: l) snd)));*) (i,s) :: l)

let make_tree tokens a =
  let sons = Int.fold 1 (Array.length a - 1) IntMap.empty (fun sons i ->
    let tid,sl,sem = a.(i) in
    let sl = clean_coord_deps sl in
    let b = Xlist.size sl > 1 in
    Xlist.fold sl sons (fun sons (super,label) ->
      IntMap.add_inc sons super [i,tid,label,b,sem] (fun l -> (i,tid,label,b,sem) :: l))) in
  make_tree_rec tokens 0 0 "" false "" sons

let rec split_sons pat sel rev = function
    (Dep d as t) :: l ->
      if pat = d.label then split_sons pat (t :: sel) rev l
      else split_sons pat sel (t :: rev) l
  | t :: l -> split_sons pat sel (t :: rev) l
  | [] -> sel,rev

let extract_sons pat = function
    Dep d ->
      let sel,sons = split_sons pat [] [] d.sons in
      sel,Dep{d with sons=sons}
  | _ -> failwith "extract_sons"

let get_label = function
    Dep d -> d.label
  | Coordination(label,sem,sons,coord) -> label
  | _ -> failwith "get_label"

let get_sorted_sons = function
    Dep d ->
      List.rev (Xlist.rev_map (sort_dependents d.sons) (fun t -> get_label t, t))
  | _ -> failwith "get_sorted_sons"

let set_sons sons = function
    Dep d -> Dep{d with sons=sons}
  | _ -> failwith "set_sons"

let extract_cc l = [],l

(*let extract_cc l =
  let first,rest =
    match sort_dependents l with
      first :: rest -> first,rest
    | _ -> failwith "extract_cc" in
  let cc_preconj,first =
    match get_sorted_sons first with
      ("cc:preconj",t) :: l -> [t],set_sons (Xlist.map l snd) first
    | ("punct",t1) ::  ("cc:preconj",t2) :: l -> [t1;t2],set_sons (Xlist.map l snd) first
    (* | ("cc",t) :: l ->
        print_endline (string_of_tree "" first);
        failwith ("extract_cc: " ^ (String.concat " " (Xlist.map (("cc",t) :: l) fst))) *)
    | ("punct",t) :: l ->
        print_endline (string_of_tree "" first);
        failwith ("extract_cc: " ^ (String.concat " " (Xlist.map (("punct",t) :: l) fst)))
    | l -> [],first in
  cc_preconj,  first :: rest*)


(*let extract_cc l =
  let first,middle,last =
    match sort_dependents l with
      [first;last] -> first,[],last
    | first :: l ->
        (match List.rev l with
          last :: rev_middle -> first,List.rev rev_middle,last
        | _ -> failwith "extract_cc")
    | _ -> failwith "extract_cc" in
  let cc_preconj,first = extract_sons "cc:preconj" first in
  if Xlist.size cc_preconj > 1 then failwith "extract_cc: cc:preconj" else
  let cc,last = extract_sons "cc" last in
  let punct,last = extract_sons "punct" last in
  if Xlist.size cc > 1 then failwith "extract_cc: cc" else
  if Xlist.size punct > 1 then failwith "extract_cc: punct 1" else
  let puncts,middle =
    Xlist.fold middle ([],[]) (fun (puncts,middle) t ->
      let punct,t = extract_sons "punct" t in
      if Xlist.size punct > 1 then failwith "extract_cc: punct 2" else
     punct @ puncts, t :: middle) in
  sort_dependents (cc_preconj @ cc @ punct @ puncts),
  [first] @ (List.rev middle) @ [last]*)

let rec process_coordination = function
    Dep d ->
      let sons = Xlist.rev_map d.sons process_coordination in
      let coord,sons = split_sons "conj" [] [] sons in
      if coord = [] then Dep{d with sons=sons} else
      let coord,sons = extract_cc (Dep{d with sons=sons} :: coord) in
      Coordination(d.label,d.sem,sons,coord)
  | _ -> failwith "process_coordination"

(*let rec shift_case = function
    Dep(id,tid,lci,label,sem,sons,is_shared) as t ->
      let case,sons = split_sons "case" [] [] sons in
      (match case with
        [] -> Dep(id,tid,lci,label,sem,Xlist.rev_map sons shift_case,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2)] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_case",sem2,Xlist.rev_map sons shift_case,is_shared2) :: sons2,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2);t2] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_case",sem2,Xlist.rev_map (t2 :: sons) shift_case,is_shared2) :: sons2,is_shared)
      | _ -> print_endline (string_of_tree "" t); failwith "shift_case")
  | Coordination(label,sem,sons,coords) -> Coordination(label,sem,Xlist.rev_map sons shift_case,coords)

let rec shift_nummod = function
    Dep(id,tid,lci,label,sem,sons,is_shared) as t ->
      let nummod,sons = split_sons "nummod" [] [] sons in
      (match nummod with
        [] -> Dep(id,tid,lci,label,sem,Xlist.rev_map sons shift_nummod,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2)] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_nummod",sem2,Xlist.rev_map sons shift_nummod,is_shared2) :: sons2,is_shared)
      | _ -> print_endline (string_of_tree "" t); failwith "shift_nummod")
  | Coordination(label,sem,sons,coords) -> Coordination(label,sem,Xlist.rev_map sons shift_nummod,coords)

let rec shift_mark = function
    Dep(id,tid,lci,label,sem,sons,is_shared) as t ->
      let mark,sons = split_sons "mark" [] [] sons in
      (match sort_dependents mark with
        [] -> Dep(id,tid,lci,label,sem,Xlist.rev_map sons shift_mark,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2)] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_mark",sem2,Xlist.rev_map sons shift_mark,is_shared2) :: sons2,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2);t2] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_mark",sem2,Xlist.rev_map (t2 :: sons) shift_mark,is_shared2) :: sons2,is_shared)
      (* | [Dep(_,_,(lem,_,_),_,_,_,_);Dep(_,_,(lem2,_,_),_,_,_,_)] -> print_endline (string_of_tree "" t); failwith ("shift_mark: " ^ lem ^ " " ^ lem2) *)
      | _ -> print_endline (string_of_tree "" t); failwith "shift_mark")
  | Coordination(label,sem,sons,coords) -> Coordination(label,sem,Xlist.rev_map sons shift_mark,coords)

let rec shift_cop = function
    Dep(id,tid,lci,label,sem,sons,is_shared) as t ->
      let cop,sons = split_sons "cop" [] [] sons in
      (match cop with
        [] -> Dep(id,tid,lci,label,sem,Xlist.rev_map sons shift_cop,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2)] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_cop",sem2,Xlist.rev_map sons shift_cop,is_shared2) :: sons2,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2);t2] ->
          Dep(id2,tid2,lci2,label,sem,Dep(id,tid,lci,"rev_cop",sem2,Xlist.rev_map (t2 :: sons) shift_cop,is_shared2) :: sons2,is_shared)
      | _ -> print_endline (string_of_tree "" t); failwith "shift_cop")
  | Coordination(label,sem,sons,coords) -> Coordination(label,sem,Xlist.rev_map sons shift_cop,coords)

let rec shift_aux_pass = function
    Dep(id,tid,lci,label,sem,sons,is_shared) as t ->
      let aux_pass,sons = split_sons "aux:pass" [] [] sons in
      (match aux_pass with
        [] -> Dep(id,tid,lci,label,sem,Xlist.rev_map sons shift_aux_pass,is_shared)
      | [Dep(id2,tid2,lci2,label2,sem2,sons2,is_shared2)] ->
          Dep(id2,tid2,lci2,label,sem2,Dep(id,tid,lci,"rev_aux:pass",sem,Xlist.rev_map sons shift_aux_pass,is_shared2) :: sons2,is_shared)
      | _ -> print_endline (string_of_tree "" t); failwith "shift_aux_pass")
  | Coordination(label,sem,sons,coords) -> Coordination(label,sem,Xlist.rev_map sons shift_aux_pass,coords)*)

let make_trees corpus =
  Xlist.rev_map corpus (fun (conll_sentence, text, orig, tokens) ->
    (* try *)
    let a = match conll_sentence.sentence with
        DepSentence[a] -> a
      | _ -> failwith "list_dependencies" in
    let tree = make_tree tokens a in
    let tree = process_coordination tree in
(*    let tree = shift_case tree in
    let tree = shift_nummod tree in
    let tree = shift_mark tree in
    let tree = shift_cop tree in
    let tree = shift_aux_pass tree in*)
    (* print_endline conll_sentence.id;
    print_endline text;
    print_endline (string_of_tree "" tree); *)
    conll_sentence.id,text,tree,tokens
    (*with e -> (print_endline (Printexc.to_string e);
    print_endline conll_sentence.id;
    print_endline text;
    (* print_endline (string_of_tree "" tree); *)
    ())*))

let rec flatten_coordination is_coord ulabel usem = function
    Dep d ->
      if ulabel = "" then [is_coord,Dep d] else [is_coord,Dep{d with label=ulabel;sem=usem}]
  | Coordination(label,sem,sons,coords) ->
      if ulabel = "" then List.flatten (Xlist.rev_map sons (flatten_coordination true label sem))
      else List.flatten (Xlist.rev_map sons (flatten_coordination true ulabel usem))
  | _ -> failwith "flatten_coordination"

let string_of_dependency2 is_coord (lemma1,cat1,interp1) label sem (lemma2,cat2,interp2) =
  (if is_coord then "COORD " else "") ^
  lemma1 ^ ":" ^ cat1 ^ ":" ^ ENIAMtagset.render interp1 ^
  " -> " ^ label ^ (if sem = "" then "" else "["^sem^"]") ^ " -> "
  (*^ lemma2 ^ ":"*) ^ cat2 ^ ":" ^ ENIAMtagset.render interp2

type sel = Any | Value of string list | Agr of string
type coord = Coord | Gen
type pattern =
    PatternNode of sel * sel * sel list (* (sel * pattern) list *)
  | PatternPhrase of sel * sel list
  | PatternEdge of pattern * sel * pattern

let phrase_names = StringSet.of_list ["np";"adjp";"ip";"infp";"pp";"comprep";"sent";"cp";"conjp"]

let raw_patterns = File.load_lines "data/patterns.tab"
let raw_pair_patterns = File.load_lines "data/pair_patterns.tab"

let is_phrase = function
    Value[a] :: _ -> StringSet.mem phrase_names a
  | _ -> false

let parse_pattern2 s a =
  let l = Xlist.map (Xstring.split ":" a) (function
      "_" -> Any
    | "$l" -> Agr "l"
    | "$n" -> Agr "n"
    | "$c" -> Agr "c"
    | "$g" -> Agr "g"
    | "$p" -> Agr "p"
    | "." -> Value ["."]
    | t -> Value(Xstring.split "\\." t)) in
  if l = [] then failwith ("parse_pattern2: " ^ s) else
  if is_phrase l then
    match l with
      phrase :: interp -> PatternPhrase(phrase,interp)
    | _ -> failwith ("parse_pattern2: " ^ s)
  else
    match l with
      lemma :: cat :: interp -> PatternNode(lemma,cat,interp)
    | _ -> failwith ("parse_pattern2: " ^ s)

let parse_phrase s =
  match parse_pattern2 s s with
    PatternPhrase(Value [phrase],interp) -> phrase,interp
  | _ -> failwith "parse_phrase"

let parse_pattern s =
  if s = "" then [] else
  if String.get s 0 = '#' then [] else
  match Xstring.split " " s with
    [a;"->";"_";"->";b] -> [Gen,s,parse_pattern2 s a,Any,parse_pattern2 s b]
  | [a;"->";label;"->";b] -> [Gen,s,parse_pattern2 s a,Value [label],parse_pattern2 s b]
  | [a;"->";label;"->";"[";b1;"->";label_b;"->";b2;"]"] -> [Gen,s,parse_pattern2 s a,Value [label],PatternEdge(parse_pattern2 s b1,Value [label_b],parse_pattern2 s b2)]
  | [a;"->";label;"->";"[";b;"->";label_b1;"->";b1;"|";label_b2;"->";b2;"]"] ->
       [Gen,s,parse_pattern2 s a,Value [label],PatternEdge(PatternEdge(parse_pattern2 s b,Value [label_b1],parse_pattern2 s b1),Value [label_b2],parse_pattern2 s b2)]
  | ["[";a1;"->";label_a;"->";a2;"]";"->";label;"->";b] -> [Gen,s,PatternEdge(parse_pattern2 s a1,Value [label_a],parse_pattern2 s a2),Value [label],parse_pattern2 s b]
  | ["COORD";a;"->";"_";"->";b] -> [Coord,s,parse_pattern2 s a,Any,parse_pattern2 s b]
  | ["COORD";a;"->";label;"->";b] -> [Coord,s,parse_pattern2 s a,Value [label],parse_pattern2 s b]
  | ["COORD";a;"->";label;"->";"[";b1;"->";label_b;"->";b2;"]"] -> [Coord,s,parse_pattern2 s a,Value [label],PatternEdge(parse_pattern2 s b1,Value [label_b],parse_pattern2 s b2)]
  | _ -> failwith ("parse_pattern: " ^ s)

let parse_pair_pattern s =
  if s = "" then [] else
  if String.get s 0 = '#' then [] else
  match Xstring.split "\t" s with
    [phrase;pat] ->
      let phrase,interp = parse_phrase phrase in
      (match parse_pattern pat with
        [coord,s,p1,plabel,p2] -> [(phrase,interp),coord,s,p1,plabel,p2]
      | _ -> failwith ("parse_pair_pattern 1: " ^ s))
  | _ -> failwith ("parse_pair_pattern 2: " ^ s)

let patterns = List.flatten (Xlist.rev_map raw_patterns parse_pattern)
let pair_patterns = List.flatten (Xlist.rev_map raw_pair_patterns parse_pair_pattern)

let match_string map s = function
    Any -> (*print_endline ("match_string: Any " ^ s);*) map
  | Value l ->
      let b = Xlist.fold l false (fun b t -> s = t || b) in
      (*print_endline ("match_string: " ^ t ^ " " ^ s);*)
      if b then map else raise Not_found
  | Agr n ->
      if StringMap.mem map n then
        if StringMap.find map n = s then map else raise Not_found
      else StringMap.add map n s

let rec match_interp_rec2 map = function
    [s],pat -> match_string map s pat
  | ["congr";"rec"],pat -> map
  | _,pat -> failwith "match_interp_rec2"

let rec match_interp_rec map = function
    s :: l,ps :: pl ->
       let map = match_interp_rec2 map (s,ps) in
       match_interp_rec map (l,pl)
  | _,[] -> map
  | _ -> failwith "match_interp_rec"

let match_interp map interp pinterp =
  match interp with
    [interp] -> match_interp_rec map (interp,pinterp)
  | _ -> failwith "match_interp"

let rec match_pattern_rec map = function
    phrase,Dep({sons=[]} as d),PatternNode(plemma,pcat,pinterp) ->
      (* print_endline ("match_pattern_rec 1: \n" ^ string_of_tree "" (Dep d)); *)
      let map = match_string map d.lemma plemma in
      let map = match_string map d.cat pcat in
      let map = match_interp map d.interp pinterp in
      map
  | (phrase,interp),Dep d,PatternPhrase(pphrase,pinterp) ->
      (* print_endline ("match_pattern_rec 1: \n" ^ string_of_tree "" (Dep d)); *)
      let map = match_string map phrase pphrase in
      let map = match_interp map interp pinterp in
      map
  | phrase,Dep({sons=[Dep d1;Dep d2]} as d),PatternEdge(PatternEdge(p,plabel1,p1),plabel2,p2) ->
      (* print_endline ("match_pattern_rec 2: \n" ^ string_of_tree "" (Dep d1)); *)
      let map = match_pattern_rec map (("",[]),Dep {d with sons=[]},p) in
      (try
        let map = match_string map d1.label plabel1 in
        let map = match_pattern_rec map (("",[]),Dep d1,p1) in
        let map = match_string map d2.label plabel2 in
        let map = match_pattern_rec map (("",[]),Dep d2,p2) in
        map
      with Not_found -> (
        let map = match_string map d1.label plabel2 in
        let map = match_pattern_rec map (("",[]),Dep d1,p2) in
        let map = match_string map d2.label plabel1 in
        let map = match_pattern_rec map (("",[]),Dep d2,p1) in
        map))
  | phrase,Dep({sons=[Dep d2]} as d1),PatternEdge(p1,plabel,p2) ->
      (* print_endline ("match_pattern_rec 2: \n" ^ string_of_tree "" (Dep d1)); *)
      let map = match_pattern_rec map (("",[]),Dep {d1 with sons=[]},p1) in
      let map = match_string map d2.label plabel in
      let map = match_pattern_rec map (("",[]),Dep d2,p2) in
      map
  | _ -> raise Not_found


let rec match_pattern is_coord (phrase1,d1) (phrase2,d2) = function
  (coord,s,p1,plabel,p2) :: l ->
    (* print_endline s; *)
    if is_coord || d2.is_shared || coord = Gen then
      try
        let map = StringMap.empty in
        let map = match_pattern_rec map (phrase1,Dep d1,p1) in
        let map = match_string map d2.label plabel in
        let _ = match_pattern_rec map (phrase2,Dep d2,p2) in
        s
      with Not_found -> match_pattern is_coord (phrase1,d1) (phrase2,d2) l
    else match_pattern is_coord (phrase1,d1) (phrase2,d2) l
  | [] -> raise Not_found

let match_phrase_interp s map pinterp =
  let interp = Xlist.rev_map pinterp (function
      Value [v] -> [v]
    | Agr v -> (try [StringMap.find map v] with Not_found -> failwith ("match_phrase_interp: " ^ s))
    | _ -> failwith ("match_phrase_interp: " ^ s)) in
  [List.rev interp]

let rec match_pair_pattern is_coord (phrase1,d1) (phrase2,d2) = function
  ((pphrase,pinterp),coord,s,p1,plabel,p2) :: l ->
    (* print_endline s; *)
    if is_coord || d2.is_shared || coord = Gen then
      try
        let map = StringMap.empty in
        let map = match_pattern_rec map (phrase1,Dep d1,p1) in
        let map = match_string map d2.label plabel in
        let map = match_pattern_rec map (phrase2,Dep d2,p2) in
        pphrase, match_phrase_interp s map pinterp, s
      with Not_found -> match_pair_pattern is_coord (phrase1,d1) (phrase2,d2) l
    else match_pair_pattern is_coord (phrase1,d1) (phrase2,d2) l
  | [] -> raise Not_found

let rec fold_tree tree s f =
  match tree with
      Dep d -> Xlist.fold d.sons (f s (Dep d)) (fun s t -> fold_tree t s f)
    | Coordination(label,sem,sons,coords) as t -> Xlist.fold sons (f s t) (fun s t -> fold_tree t s f)
    | _ -> failwith "fold_tree"

(*let list_dependencies_tree corpus =
  Xlist.fold corpus StringQMap.empty (fun qmap (sentence_id, text, tree, tokens) ->
    fold_tree tree qmap (fun qmap -> function
      Dep d ->
        Xlist.fold (List.flatten (Xlist.rev_map d.sons (flatten_coordination false "" ""))) qmap (fun qmap -> function
            is_coord,Dep d2 ->
              (try
                let s = match_pattern is_coord (Dep d) (Dep d2) patterns in
                StringQMap.add qmap ("PATTERN " ^ s)
              with Not_found -> StringQMap.add qmap (string_of_dependency2 is_coord (d.lemma,d.cat,d.interp)  d2.label d2.sem (d2.lemma,d2.cat,d2.interp)))
          | _ -> failwith "list_dependencies_tree")
    | Coordination(label,sem,sons,coords) -> StringQMap.add qmap "Coordination"
    | _ -> failwith "list_dependencies_tree"))

let list_dependencies_tree2 corpus =
  Xlist.fold corpus StringMap.empty (fun map (sentence_id, text, tree, tokens) ->
    fold_tree tree map (fun map -> function
      Dep d ->
        Xlist.fold (List.flatten (Xlist.rev_map d.sons (flatten_coordination false "" ""))) map (fun map -> function
            is_coord,Dep d2 ->
              (try
                let _ = match_pattern is_coord (Dep d) (Dep d2) patterns in
                map
              with Not_found -> StringMap.add_inc map (string_of_dependency2 is_coord (d.lemma,d.cat,d.interp)  d2.label d2.sem (d2.lemma,d2.cat,d2.interp)) [text] (fun l -> text :: l))
          | _ -> failwith "list_dependencies_tree2")
    | Coordination(label,sem,sons,coords) -> StringMap.add_inc map "Coordination" [text] (fun l -> text :: l)
    | _ -> failwith "list_dependencies_tree2"))*)

let rec parse_pair_patterns = function
    Cluster(phrase,d,l),[] -> false,Cluster(phrase,d,l)
  | Cluster(phrase,d,l), Cluster(phrase2,d2,[]) :: sons ->
              (try
                let pphrase,pinterp,_ = match_pair_pattern false (phrase,d) (phrase2,d2) pair_patterns in
                true,Cluster((pphrase,pinterp),{d with sons=Dep d2 :: d.sons},sons @ l)
              with Not_found -> parse_pair_patterns (Cluster(phrase,d,Cluster(phrase2,d2,[]) :: l), sons))
  | Cluster(phrase,d,l), t :: sons -> parse_pair_patterns (Cluster(phrase,d,t :: l), sons)
  | _ -> failwith "parse_pair_patterns"

let rec check_cc = function
    [] -> true
  | Cluster(_,{lemma=",";cat="interp";label="punct"},[]) :: l -> check_cc l
  (* | Dep{lemma="-";cat="interp";label="punct"} :: l -> check_cc l *)
  | Cluster(_,{lemma="i";cat="conj";label="cc"},[])  :: l -> check_cc l
  | Cluster(_,{lemma="a";cat="conj";label="cc"},[]) :: l -> check_cc l
  | Cluster(_,{lemma="zarówno";cat="conj";label="cc:preconj"},[]) :: l -> check_cc l
  | Cluster(_,{lemma="jak";cat="conj";label="cc";sons=[Dep{lemma="i";cat="conj"}]},_) :: l -> check_cc l
  | _ -> false

let parse_coordination = function
    Coordination(label,sem,[
      Cluster(_,{cat="adja";sons=[]},[]);
      Cluster(phrase,({cat=adj;sons=[]} as d),[Cluster(_,{lemma="-";cat="interp";label="punct"},[])])],[]) ->
        Cluster(phrase,{d with label=label;sem=sem},[])
  | Coordination(label,sem,sons,[]) ->
      let b = Xlist.fold sons true (fun b -> function
          Cluster(_,_,sons) -> check_cc sons && b
        (* | PairDep(d,_) -> check_cc d.sons && b *)
        | _ -> failwith "parse_coordination 2") in
      if b then
        match List.hd sons with
          Cluster(phrase,d,_) -> Cluster(phrase,{d with is_shared=true},[])
        (* | PairDep(d,d2) -> PairDep({d with is_shared=true; sons=[]},d2) *)
        | _ -> failwith "parse_coordination 3"
      else Coordination(label,sem,sons,[])
  | _ -> failwith "parse_coordination"

let make_phrase = function
    "subst" -> "np"
  | "depr" -> "np"
  | "ppron12" -> "np"
  | "ppron3" -> "np"
  | "ger" -> "np"
  | "adj" -> "adjp"
  | "pact" -> "adjp"
  | "ppas" -> "adjp"
  | "fin" -> "ip"
  | "bedzie" -> "ip"
  | "praet" -> "ip"
  | "winien" -> "ip"
  | "impt" -> "ip"
  | "imps" -> "ip"
  | "pred" -> "ip"
  (* | "siebie" -> "np" *)
  (* | "symbol" -> "noun"
  | "unk" -> "noun"
  | "xxx" -> "noun"
  | "adjc" -> "adj"
  | "adjp" -> "adj"
  | "adja" -> "adj"
  | "ordnum" -> "ordnum" *)
  | "inf" -> "infp"
  (* | "pcon" -> "verb"
  | "pant" -> "verb"
  | "pacta" -> "verb" *)
  | "conj" -> "conjp"
  (* | "fixed" -> "fixed"
  | "num" -> "num"*)
  | _ -> ""

let rec parse_tree = function
    Dep d ->
      (* Printf.printf "parse_tree 1: |sons|=%d\n" (Xlist.size d.sons); *)
      let sons = Xlist.rev_map d.sons parse_tree in
      (* Printf.printf "parse_tree 2: %s |sons|=%d\n" d.lemma (Xlist.size sons); *)
      let phrase = make_phrase d.cat, d.interp in
      let sons = Xlist.fold sons [] (fun sons -> function
          Cluster(phrase2,d2,[]) as t ->
              (try
                (* print_endline "parse_tree 2a"; *)
                let _ = match_pattern false (phrase,{d with sons=[]}) (phrase2,d2) patterns in
                (* print_endline "parse_tree 2b"; *)
                sons
              with Not_found -> t :: sons)
        | t -> t :: sons) in
      let b,t = parse_pair_patterns (Cluster(phrase,{d with sons=[]},[]),sons) in
      if b then parse_tree t else t
  | Coordination(label,sem,sons,coords) ->
      parse_coordination (Coordination(label,sem,List.rev (Xlist.rev_map sons parse_tree),coords))
  | Cluster(phrase,d,sons) ->
      let sons = Xlist.fold sons [] (fun sons -> function
          Cluster(phrase2,d2,[]) ->
              (try
                let _ = match_pattern false (phrase,d) (phrase2,d2) patterns in
                sons
              with Not_found -> Cluster(phrase2,d2,[]) :: sons)
        | t -> t :: sons) in
      let b,t = parse_pair_patterns (Cluster(phrase,d,[]),sons) in
      if b then parse_tree t else t
  (* | _ -> failwith "parse_tree" *)

let is_parsed = function
    Cluster(_,{lemma="<conll_root>";sons=[]},[]) -> true
  | _ -> false

let excluded = StringSet.of_list (File.load_lines "data/excluded.tab")

let rec split_tree forest = function
    Coordination(label,sem,sons,coords) ->
      Xlist.fold sons forest split_tree
  | Cluster(phrase,d,[]) -> forest
  | Cluster(phrase,d,sons) ->
      let b = Xlist.fold sons true (fun b -> function
          Cluster(_,_,[]) -> b
        | _ -> false) in
      if b then Cluster(phrase,d,sons) :: forest else
      Xlist.fold sons forest split_tree
  | _ -> failwith "split_tree"

(* let rec rules_of_tree2 = function
    Dep({sons=[]} as d) ->
      d.lemma ^ ":" ^ d.cat ^ ":" ^ ENIAMtagset.render d.interp
  | Dep({sons=[Dep d2]} as d) ->
      "[ " ^ d.lemma ^ ":" ^ d.cat ^ ":" ^ ENIAMtagset.render d.interp ^
      " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2) ^ " ]"
  | Dep({sons=[Dep d2;Dep d3]} as d) ->
      "[ " ^ d.lemma ^ ":" ^ d.cat ^ ":" ^ ENIAMtagset.render d.interp ^
      " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2) ^ " | " ^ d3.label ^ " -> " ^ rules_of_tree2 (Dep d3) ^ " ]"
  | _ -> failwith "rules_of_tree2" *)

(* let rec rules_of_tree2 = function
    Dep({sons=[]} as d) ->
      "_:" ^ d.cat ^ ":" ^ ENIAMtagset.render d.interp
  | Dep({sons=[Dep d2]} as d) ->
      "[ _:" ^ d.cat ^ ":" ^ ENIAMtagset.render d.interp ^
      " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2) ^ " ]"
  | Dep({sons=[Dep d2;Dep d3]} as d) ->
      "[ _:" ^ d.cat ^ ":" ^ ENIAMtagset.render d.interp ^
      " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2) ^ " | " ^ d3.label ^ " -> " ^ rules_of_tree2 (Dep d3) ^ " ]"
  | _ -> failwith "rules_of_tree2" *)

let rec rules_of_tree2 = function
    Dep({sons=[]} as d) ->
      "_:" ^ d.cat (*^ ":" ^ ENIAMtagset.render d.interp*)
  | Dep({sons=[Dep d2]} as d) ->
      "[ _:" ^ d.cat ^ (*":" ^ ENIAMtagset.render d.interp ^*)
      " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2) ^ " ]"
  | Dep({sons=[Dep d2;Dep d3]} as d) ->
      "[ _:" ^ d.cat ^ (*":" ^ ENIAMtagset.render d.interp ^*)
      " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2) ^ " | " ^ d3.label ^ " -> " ^ rules_of_tree2 (Dep d3) ^ " ]"
  | _ -> failwith "rules_of_tree2"


let rules_of_tree rules = function
    Cluster(_,d,sons) ->
      Xlist.fold sons rules (fun rules -> function
          Cluster(_,d2,[]) -> (rules_of_tree2 (Dep d) ^ " -> " ^ d2.label ^ " -> " ^ rules_of_tree2 (Dep d2)) :: rules
        |  _ -> failwith "rules_of_tree")
  | _ -> failwith "rules_of_tree"


let parse corpus =
  Xlist.iter corpus (fun (sentence_id, text, tree, tokens) ->
    if StringSet.mem excluded sentence_id then () else
    (try
      let tree = parse_tree tree in
      if is_parsed tree then () (*print_endline ("PARSED: " ^ sentence_id)*) else (
      print_endline sentence_id;
      print_endline text;
      print_endline (string_of_tree "" tree);
      let forest = split_tree [] tree in
      Xlist.iter forest (fun tree ->
        (* print_endline ("\n" ^ string_of_tree "" tree); *)
        let rules = rules_of_tree [] tree in
        Xlist.iter rules print_endline))
    with e ->
      print_endline sentence_id;
      print_endline text;
      print_endline (string_of_tree "" tree);
      print_endline (Printexc.to_string e)))

let extract_rules corpus =
  Xlist.fold corpus StringQMap.empty (fun qmap (sentence_id, text, tree, tokens) ->
    if StringSet.mem excluded sentence_id then qmap else
    (try
      let tree = parse_tree tree in
      if is_parsed tree then StringQMap.add qmap "PARSED" else (
      let forest = split_tree [] tree in
      Xlist.fold forest qmap (fun qmap tree ->
        let rules = rules_of_tree [] tree in
        Xlist.fold rules qmap StringQMap.add))
    with e -> StringQMap.add qmap (Printexc.to_string e)))
