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

open Xstd

type id = {corref: string; prefix: string; suffix: string; suffix2: string; numbers: int list}

let empty_id = {corref = ""; prefix = ""; suffix = ""; suffix2 = ""; numbers = []}

let parse_id id =
  (* if String.length s = 0 then empty_id else *)
  if String.length id < 6 then failwith "parse_id: za krótkie id"  else
  let corref,id = match Xstring.split "#" id with
      [corref;id] -> corref,id
    | [id] -> "",id
    | _ -> failwith ("parse_id 1: " ^ id) in
  let prefix,id,suffix2 = match Xstring.split "_" id with
      [prefix;id] -> prefix,id,""
    | [prefix;id;suffix2] -> prefix,id,suffix2
    | _ -> failwith ("parse_id 2: " ^ id) in
  let suffix,id = match Xstring.split "-" id with
      [id;suffix] -> suffix,id
    | _ -> failwith ("parse_id 3: " ^ id) in
  let numbers =  try Xlist.map (Xstring.split "\\." id) int_of_string with _ -> failwith ("parse_id 4: " ^ id) in
  {corref=corref; prefix=prefix; suffix=suffix; suffix2=suffix2; numbers=numbers}

let process_header_type typ =
  if Xstring.check_prefix "#typ_" typ then Xstring.cut_prefix "#typ_" typ
  else failwith ("process_header_type: " ^ typ)

let process_header_channel c =
  if Xstring.check_prefix "#kanal_" c then Xstring.cut_prefix "#kanal_" c
  else failwith ("process_header_channel: " ^ c)

let load_header path name =
  match Xml.parse_file (path ^ name ^ "/header.xml") with
      Xml.Element("teiHeader",_,[Xml.Element("fileDesc",[],_);
                   Xml.Element("profileDesc",[],[Xml.Element("textClass",[],[
                     Xml.Element("catRef",["scheme","#taxonomy-NKJP-type";"target",typ],[]);
                     Xml.Element("catRef",["scheme","#taxonomy-NKJP-channel";"target",channel],[])])]);
                     Xml.Element("revisionDesc",_,_)]) ->
        process_header_type typ,process_header_channel channel
    | _ -> failwith "load_header"

let get_folders path =
  Xlist.sort (Xlist.fold (Array.to_list (Sys.readdir path)) [] (fun l folder ->
    if Sys.is_directory (path ^ folder) then folder :: l else l)) compare

let load_paragraph = function
    Xml.Element("ab",["n",_;"xml:id",id_ab],[Xml.PCData paragraph]) ->
      parse_id id_ab,paragraph
  | xml -> failwith ("load_text_entry: " ^ Xml.to_string_fmt xml)

let load_text_entry = function
    Xml.Element("div",["xml:id",id_div;"decls",_],paragraphs) ->
      parse_id id_div,List.rev (Xlist.rev_map paragraphs load_paragraph)
  | xml -> failwith ("load_text_entry: " ^ Xml.to_string_fmt xml)

let load_text path name =
  match Xml.parse_file (path ^ name ^ "/text.xml") with
      Xml.Element("teiCorpus", _,[Xml.Element("xi:include",_,_);
                   Xml.Element("TEI",[],[Xml.Element("xi:include",_,_);
                     Xml.Element("text",["xml:id","txt_text";"xml:lang","pl"],[Xml.Element("body",["xml:id","txt_body"],entries)])])]) ->
        List.rev (Xlist.rev_map entries load_text_entry)
    | _ -> failwith "load_text"

let remove_rejected rev = function
    Xml.Element("seg",["corresp",_;"nkjp:rejected","true";"xml:id",_],[]) -> rev
  | Xml.Element("seg",["corresp",_;"nkjp:nps","true";"nkjp:rejected","true";"xml:id",_],[]) -> rev
  | Xml.Element("seg",["corresp",_;"xml:id",_],[]) as xml -> xml :: rev
  | Xml.Element("seg",["corresp",_;"nkjp:nps","true";"xml:id",_],[]) as xml -> xml :: rev
  | Xml.Element("nkjp:paren",[],Xml.Element("seg",["corresp",_;"nkjp:rejected","true";"xml:id",_],[]) :: _) -> rev
  | Xml.Element("nkjp:paren",[],Xml.Element("seg",["corresp",_;"nkjp:nps","true";"nkjp:rejected","true";"xml:id",_],[]) :: _) -> rev
  | Xml.Element("nkjp:paren",[],Xml.Element("seg",["corresp",_;"xml:id",_],[]) :: _) as xml -> xml :: rev
  | Xml.Element("nkjp:paren",[],Xml.Element("seg",["corresp",_;"nkjp:nps","true";"xml:id",_],[]) :: _) as xml -> xml :: rev
  | xml -> failwith ("remove_rejected: " ^ Xml.to_string_fmt xml)

let rec load_segm_token = function
    Xml.Element("seg",["corresp",corresp;"xml:id",id_seg],[]) ->
      [corresp,false,parse_id id_seg]
  | Xml.Element("seg",["corresp",corresp;"nkjp:nps","true";"xml:id",id_seg],[]) ->
      [corresp,true,parse_id id_seg]
  | Xml.Element("nkjp:paren",[],tokens) -> List.flatten (Xlist.map tokens load_segm_token)
  | Xml.Element("choice",[],alt) as xml ->
      let alt = Xlist.fold alt [] remove_rejected in
      (match alt with
        [token] -> load_segm_token token
      | _ -> failwith ("load_segm_token 2: " ^ Xml.to_string_fmt xml))
  | xml -> failwith ("load_segm_token 1: " ^ Xml.to_string_fmt xml)

let load_segm_sentence = function
    Xml.Element("s",["xml:id",id_s],tokens) ->
      parse_id id_s,List.flatten (List.rev (Xlist.rev_map tokens load_segm_token))
  | xml -> failwith ("load_segm_sentence: " ^ Xml.to_string_fmt xml)

let load_segm_entry = function
    Xml.Element("p",["corresp",corresp;"xml:id",id_p],sentences) ->
      parse_id corresp,parse_id id_p,List.rev (Xlist.rev_map sentences load_segm_sentence)
  | xml -> failwith ("load_segm_entry: " ^ Xml.to_string_fmt xml)

let load_segmentation path name =
  match Xml.parse_file (path ^ name ^ "/ann_segmentation.xml") with
      Xml.Element("teiCorpus", _,[Xml.Element("xi:include",_,_);
                   Xml.Element("TEI",[],[Xml.Element("xi:include",_,_);
                     Xml.Element("text",["xml:id","segm_text";"xml:lang","pl"],[Xml.Element("body",["xml:id","segm_body"],entries)])])]) ->
        List.rev (Xlist.rev_map entries load_segm_entry)
    | _ -> failwith "load_segmentation"

let load_disamb = function
    Xml.Element("fs",["feats",_;"type","tool_report"],
      [Xml.Element("f",["fVal",_;"name","choice"],_);
       Xml.Element("f",["name","interpretation"],[Xml.Element("string",[],[Xml.PCData interp])])]) ->
         interp
  | xml -> failwith ("load_disamb: " ^ Xml.to_string_fmt xml)

let load_morph_token = function
    Xml.Element("seg",["corresp",corresp;"xml:id",id_seg],[Xml.Element("fs",["type","morph"],
      [Xml.Element("f",["name","orth"],[Xml.Element("string",[],[Xml.PCData orth])]);
       Xml.Element("f",["name","interps"],_);Xml.Element("f",["name","disamb"],[disamb])])]) ->
      parse_id corresp,parse_id id_seg,orth,load_disamb disamb
  | Xml.Element("seg",["corresp",corresp;"xml:id",id_seg],[Xml.Element("fs",["type","morph"],
      [Xml.Element("f",["name","orth"],[Xml.Element("string",[],[Xml.PCData orth])]);
       Xml.Element("f",["name","nps"],[Xml.Element("binary",["value","true"],[])]);
       Xml.Element("f",["name","interps"],_);Xml.Element("f",["name","disamb"],[disamb])])]) ->
      parse_id corresp,parse_id id_seg,orth,load_disamb disamb
  | xml -> failwith ("load_morph_token: " ^ Xml.to_string_fmt xml)

let load_morph_sentence = function
    Xml.Element("s",["corresp",corresp;"xml:id",id_s],tokens) ->
      parse_id corresp,parse_id id_s,List.rev (Xlist.rev_map tokens load_morph_token)
  | xml -> failwith ("load_morph_sentence: " ^ Xml.to_string_fmt xml)

let load_morph_entry = function
    Xml.Element("p",["corresp",corresp;"xml:id",id_p],sentences) ->
      parse_id corresp,parse_id id_p,List.rev (Xlist.rev_map sentences load_morph_sentence)
  | xml -> failwith ("load_morph_entry: " ^ Xml.to_string_fmt xml)

let load_morphosyntax path name =
  match Xml.parse_file (path ^ name ^ "/ann_morphosyntax.xml") with
      Xml.Element("teiCorpus", _,[Xml.Element("xi:include",_,_);
                   Xml.Element("TEI",[],[Xml.Element("xi:include",_,_);
                     Xml.Element("text",[],[Xml.Element("body",[],entries)])])]) ->
        List.rev (Xlist.rev_map entries load_morph_entry)
    | _ -> failwith "load_morphosyntax"

type named = {typ: string; orth: string; base: string; cert: string; subtype: string; derived: string*string; wheen: string; }

let empty_named = {typ=""; orth=""; base=""; cert=""; subtype=""; derived="",""; wheen=""}

let load_named_feature named = function
    Xml.Element("f",["name","type"],[Xml.Element("symbol",["value",v],[])]) -> {named with typ=v}
  | Xml.Element("f",["name","orth"],[Xml.Element("string",[],[Xml.PCData orth])]) -> {named with orth=orth}
  | Xml.Element("f",["name","base"],[Xml.Element("string",[],[Xml.PCData base])]) -> {named with base=base}
  | Xml.Element("f",["name","certainty"],[Xml.Element("symbol",["value",cert],[])]) -> {named with cert=cert}
  | Xml.Element("f",["name","subtype"],[Xml.Element("symbol",["value",v],[])]) -> {named with subtype=v}
  | Xml.Element("f",["name","derived"],[Xml.Element("fs",["type","derivation"],[
      Xml.Element("f",["name","derivType"],[Xml.Element("symbol",["value",v],[])]);
      Xml.Element("f",["name","derivedFrom"],[Xml.Element("string",[],[Xml.PCData from])])])]) -> {named with derived=(v,from)}
  | Xml.Element("f",["name","derived"],[Xml.Element("fs",["type","derivation"],[
      Xml.Element("f",["name","derivType"],[Xml.Element("symbol",["value",v],[])]);
      Xml.Element("f",["name","derivedFrom"],[Xml.Element("string",[],[])])])]) -> {named with derived=(v,"")}
  | Xml.Element("f",["name","when"],[Xml.Element("string",[],[Xml.PCData w])]) -> {named with wheen=w}
  | Xml.Element("f",["name","when"],[Xml.Element("string",[],[])]) -> {named with wheen=""}
  | Xml.Element("f",["name","comment"],[Xml.Element("string",[],[Xml.PCData base])]) -> named
  | Xml.Element("f",["name","comment"],[Xml.Element("string",[],[])]) -> named
  | xml -> failwith ("load_named_feature: " ^ Xml.to_string_fmt xml)

let load_ptr = function
  | Xml.Element("ptr",["target",target],[]) -> parse_id target
  | xml -> failwith ("load_ptr: " ^ Xml.to_string_fmt xml)

let load_named_token = function
    Xml.Element("seg",["xml:id",id_seg],Xml.Element("fs",["type","named"],features) :: ptrs) ->
      let named = Xlist.fold features empty_named load_named_feature in
      let ptrs = Xlist.fold ptrs [] (fun ptrs xml -> load_ptr xml :: ptrs) in
      parse_id id_seg,named,List.rev ptrs
  | xml -> failwith ("load_named_token: " ^ Xml.to_string_fmt xml)

let load_named_sentence = function
    Xml.Element("s",["xml:id",id_s;"corresp",corresp],tokens) ->
      parse_id corresp,parse_id id_s,List.rev (Xlist.rev_map tokens load_named_token)
  | xml -> failwith ("load_morph_sentence: " ^ Xml.to_string_fmt xml)

let load_named_entry = function
    Xml.Element("p",["xml:id",id_p;"corresp",corresp],sentences) ->
      parse_id corresp,parse_id id_p,List.rev (Xlist.rev_map sentences load_named_sentence)
  | xml -> failwith ("load_morph_entry: " ^ Xml.to_string_fmt xml)

let load_named path name =
  try
  match Xml.parse_file (path ^ name ^ "/ann_named.xml") with
      Xml.Element("teiCorpus", _,[Xml.Element("xi:include",_,_);
                   Xml.Element("TEI",[],[Xml.Element("xi:include",_,_);
                     Xml.Element("text",["xml:lang","pl"],[Xml.Element("body",[],entries)])])]) ->
        List.rev (Xlist.rev_map entries load_named_entry)
    | _ -> failwith "load_morphosyntax"
  with Xml.File_not_found _ -> []

let parse_seg_corresp corresp =
  if not (Xstring.check_prefix "text.xml#string-range(" corresp) then failwith "parse_seg_corresp" else
  if not (Xstring.check_sufix ")" corresp) then failwith "parse_seg_corresp" else
  let corresp = Xstring.cut_prefix "text.xml#string-range(" corresp in
  let corresp = Xstring.cut_sufix ")" corresp in
  let id,beg,len = match Xstring.split "," corresp with
    [id;beg;len] -> parse_id id, int_of_string beg, int_of_string len
  | _ -> failwith "parse_seg_corresp" in
  let id_div,id_ab = match id with
    {corref=""; prefix="txt"; numbers=[id_div;id_ab]; suffix="ab"} -> id_div,id_ab
  | _ -> failwith "parse_seg_corresp" in
  id_div,id_ab,beg,len

let pos_set = StringSet.of_list
         ["subst";"depr";"ppron12";"ppron3";"siebie";"prep";"adj";"adjc";"adjp";"adja";"num";
          "adv";"ger";"pact";"ppas";"fin";"bedzie";"praet";"winien";"impt";
          "imps";"pred";"aglt";"inf";"pcon";"pant";"qub";"comp";"conj";"interj";"burk";"interp";
          "brev";"xxx";"numcol"]

let parse_disamb disamb =
  if disamb = "::interp" then ":","interp",[] else
  if disamb = ":-):interp" then ":-)","interp",[] else
  (* if Xstring.check_sufix ":interp" disamb then  Xstring.cut_sufix ":interp" disamb, "interp", [] else *)
  match Xstring.split_delim ":" disamb with
    lemma1 :: lemma2 :: "subst" :: interp -> lemma1 ^ ":" ^ lemma2,"subst",interp
  | lemma1 :: lemma2 :: lemma3 :: "subst" :: interp -> lemma1 ^ ":" ^ lemma2 ^ ":" ^ lemma3,"subst",interp
  | lemma :: pos :: interp ->
        if StringSet.mem pos_set pos then lemma,pos,interp
        else failwith ("parse_disamb: " ^ disamb)
  | _ -> failwith "parse_disamb"

let rec merge_tokens name id_p rev = function
    (corresp,nps,{corref=""; prefix="segm"; numbers=[id_segm_p;id_segm_s]; suffix="seg"}) :: segmentation,
    ({corref="ann_segmentation.xml"; prefix="segm"; numbers=[c_segm_p;c_segm_s]; suffix="seg"},
     {corref=""; prefix="morph"; numbers=[id_morph_p;id_morph_s]; suffix="seg"},orth,disamb) :: morphosyntax ->
        (* if id_p <> id_segm_p then Printf.printf "merge_tokens inconsistent numbering: %s segm_%d-p segm_%d.%d-s\n" name id_p id_segm_p id_segm_s; *)
        if id_segm_p <> c_segm_p || id_segm_p <> id_morph_p then failwith "merge_tokens 2" else
        if id_segm_s <> c_segm_s || c_segm_s <> id_morph_s then failwith "merge_tokens 3" else
        let id_div,id_ab,beg,len = parse_seg_corresp corresp in(
        (* if id_div <> id_p then (*failwith*)print_endline (Printf.sprintf "merge_tokens 4: %s %d %s" name id_p corresp); (*else*) *)
        let lemma,cat,interp = parse_disamb disamb in
        merge_tokens name id_p ((id_div,id_ab,(beg,len,nps,orth,lemma,cat,interp)) :: rev) (segmentation,morphosyntax))
  | [],[] -> List.rev rev
  | _ -> failwith "merge_tokens 1"

let rec split_sentences id_div id_ab rev rev2 = function
    (id_div2,id_ab2,token) :: l ->
       if id_div = id_div2 && id_ab = id_ab2 then split_sentences id_div id_ab (token :: rev) rev2 l else
       split_sentences id_div2 id_ab2 [token] ((id_div,id_ab,List.rev rev) :: rev2) l
  | [] -> List.rev ((id_div,id_ab,List.rev rev) :: rev2)

let print_tokens tokens =
  Xlist.iter tokens (fun (_,_,(beg,len,nps,orth,lemma,cat,interp)) ->
    Printf.printf "beg=%d len=%d %s %s\n" beg len orth lemma)

let rec merge_sentences name id_p rev = function
    ({corref=""; prefix="segm"; numbers=[id_segm_p;id_segm_s]; suffix="s"},segm_tokens) :: segmentation,
    ({corref="ann_segmentation.xml"; prefix="segm"; numbers=[c_segm_p;c_segm_s]; suffix="s"},
     {corref=""; prefix="morph"; numbers=[id_morph_p;id_morph_s]; suffix="s"},morph_tokens) :: morphosyntax,
    ({corref="ann_morphosyntax.xml"; prefix="morph"; numbers=[c_morph_p;c_morph_s]; suffix="s"},
     {corref=""; prefix="named"; numbers=[id_named_p;id_named_s]; suffix="s"},named_tokens) :: named ->
        (* if id_p <> id_segm_p then Printf.printf "merge_sentences inconsistent numbering: %s segm_%d-p segm_%d.%d-s\n" name id_p id_segm_p id_segm_s; *)
        if id_segm_p <> c_segm_p || id_segm_p <> id_morph_p || id_segm_p <> c_morph_p || id_segm_p <> id_named_p then failwith "merge_sentences 2" else
        if id_segm_s <> c_segm_s || c_segm_s <> id_morph_s || c_segm_s <> c_morph_s || c_segm_s <> id_named_s then failwith "merge_sentences 3" else
        let tokens = merge_tokens name id_p [] (segm_tokens,morph_tokens) in
        (* let _ = print_tokens tokens in *)
        let id_s = string_of_int id_segm_p ^ "." ^ string_of_int id_segm_s in
        if tokens = [] then failwith "merge_sentences 4" else
        let id_div,id_ab,token = List.hd tokens in
        let l = match split_sentences id_div id_ab [token] [] (List.tl tokens) with
          [id_div,id_ab,tokens] -> [id_div,id_ab,id_s,tokens]
        | [id_div1,id_ab1,tokens1;id_div2,id_ab2,tokens2] -> [id_div2,id_ab2,id_s^"b",tokens2;id_div1,id_ab1,id_s^"a",tokens1]
        | [id_div1,id_ab1,tokens1;id_div2,id_ab2,tokens2;id_div3,id_ab3,tokens3] -> [id_div3,id_ab3,id_s^"c",tokens3;id_div2,id_ab2,id_s^"b",tokens2;id_div1,id_ab1,id_s^"a",tokens1]
        | _ -> failwith (Printf.sprintf "merge_sentences 5: %s %d %d" name id_div id_ab) in
        let named_tokens = Xlist.fold named_tokens [] (fun named_tokens (id,n,ptrs) ->
          (StringSet.of_list (Xstring.split " " n.orth),id,n,ptrs) :: named_tokens) in
        let l = Xlist.map l (fun (id_div,id_ab,id_s,tokens) ->
          let orths = Xlist.fold tokens StringSet.empty (fun orths (_,_,_,orth,_,_,_) -> StringSet.add orths orth) in
          let named_tokens = Xlist.fold named_tokens [] (fun named_tokens (n_orths,id,n,ptrs) ->
            if StringSet.size (StringSet.intersection orths n_orths) = StringSet.size n_orths then (id,n,ptrs) :: named_tokens else named_tokens) in
          id_div,id_ab,id_s,tokens,named_tokens) in
        merge_sentences name id_p (l @ rev) (segmentation,morphosyntax,named)
  | ({corref=""; prefix="segm"; numbers=[id_segm_p;id_segm_s]; suffix="s"},segm_tokens) :: segmentation,
    ({corref="ann_segmentation.xml"; prefix="segm"; numbers=[c_segm_p;c_segm_s]; suffix="s"},
     {corref=""; prefix="morph"; numbers=[id_morph_p;id_morph_s]; suffix="s"},morph_tokens) :: morphosyntax, [] ->
        (* if id_p <> id_segm_p then Printf.printf "merge_sentences inconsistent numbering: %s segm_%d-p segm_%d.%d-s\n" name id_p id_segm_p id_segm_s; *)
        if id_segm_p <> c_segm_p || id_segm_p <> id_morph_p then failwith "merge_sentences 2" else
        if id_segm_s <> c_segm_s || c_segm_s <> id_morph_s then failwith "merge_sentences 3" else
        let tokens = merge_tokens name id_p [] (segm_tokens,morph_tokens) in
        (* let _ = print_tokens tokens in *)
        let id_s = string_of_int id_segm_p ^ "." ^ string_of_int id_segm_s in
        if tokens = [] then failwith "merge_sentences 4" else
        let id_div,id_ab,token = List.hd tokens in
        let l = match split_sentences id_div id_ab [token] [] (List.tl tokens) with
          [id_div,id_ab,tokens] -> [id_div,id_ab,id_s,tokens]
        | [id_div1,id_ab1,tokens1;id_div2,id_ab2,tokens2] -> [id_div2,id_ab2,id_s^"b",tokens2;id_div1,id_ab1,id_s^"a",tokens1]
        | [id_div1,id_ab1,tokens1;id_div2,id_ab2,tokens2;id_div3,id_ab3,tokens3] -> [id_div3,id_ab3,id_s^"c",tokens3;id_div2,id_ab2,id_s^"b",tokens2;id_div1,id_ab1,id_s^"a",tokens1]
        | _ -> failwith (Printf.sprintf "merge_sentences 5: %s %d %d" name id_div id_ab) in
        let l = Xlist.map l (fun (id_div,id_ab,id_s,tokens) -> id_div,id_ab,id_s,tokens,[]) in
        merge_sentences name id_p (l @ rev) (segmentation,morphosyntax,[])
  | [],[],[] -> List.rev rev
  | _ -> failwith "merge_sentences"

let rec merge_paragraph id_div id_ab rev = function
    (id_div2,id_ab2,id_s,tokens,named_tokens) :: sentences ->
      if id_div <> id_div2 || id_ab <> id_ab2 then List.rev rev, (id_div2,id_ab2,id_s,tokens,named_tokens) :: sentences
      else merge_paragraph id_div id_ab ((id_s,tokens,named_tokens) :: rev) sentences
  | [] -> List.rev rev, []

let rec get_spaces n = function
    " " :: p -> get_spaces (n+1) p
  | "\194\160" :: p -> get_spaces (n+1) p
  | p -> n,p

let rec split_front rev n p =
  if n = 0 then List.rev rev, p else
  split_front (List.hd p :: rev) (n-1) (List.tl p)

let rec combine_three = function
    [],[],[] -> []
  | x1 :: l1, x2 :: l2, x3 :: l3 -> (x1,x2,x3) :: combine_three (l1,l2,l3)
  | _ -> failwith "combine_three"

type split = Single of string | Split of (string * string * string * string list) list | Correct

type err = Err | ErrTagE | TagE | TErr | DErr | CErr | Corr

let parse_err = function
    "ERR" -> Err
  | "ERR-TAGE" -> ErrTagE
  | "TAGE" -> TagE
  | "TERR" -> TErr
  | "DERR" -> DErr
  | "CERR" -> CErr
  | s -> failwith ("parse_err: " ^ s)

let load_err_corr err_corr_filename =
  File.fold_tab err_corr_filename StringMap.empty (fun err_corr -> function
    [real_orth;lemma;orth;interp;freq;compos;sgjp;common;err] ->
      StringMap.add_inc err_corr (real_orth^"\t"^lemma^"\t"^interp) (Single orth,parse_err err) (fun _ -> failwith "load_err_corr")
  | [real_orth;lemma;interp;split_orth;split_lemma;split_interp;freq;compos;sgjp;common;err] ->
      let l = combine_three (Xstring.split "|" split_orth,Xstring.split "|" split_lemma,Xstring.split "|" split_interp) in
      let l = Xlist.map l (fun (orth,lemma,interp) ->
        match Xstring.split ":" interp with
          cat :: interp -> orth,lemma,cat,interp
        | _ -> failwith ("load_err_corr: " ^ interp)) in
      StringMap.add_inc err_corr (real_orth^"\t"^lemma^"\t"^interp) (Split l,parse_err err) (fun _ -> failwith "load_err_corr")
  | l -> print_endline ("load_err_corr: " ^ String.concat "\t" l); err_corr)
    (* oooo	o	o	interj	1	NCOMPOS	NON-SGJP	CW	DERR
    o	opylać	opyla	fin:sg:ter:imperf	1	NCOMPOS	NON-SGJP	CW	ERR *)
    (* napewno  napewno qub     na|pewno        na|pewno        prep:acc|adv:pos        2       NCOMPOS NON-SGJP        CW      ERR *)

(* let err_corr = load_err_corr "../resources/NKJP1M/NKJP1M-frequency-with-corrections.tab" *)
let err_corr = load_err_corr "data/spelling-corrections.tab"

let match_tokens name id_p s sentences =
  let p = Xunicode.utf8_chars_of_utf8_string s in
  let len = Xlist.size p in
  let i,p,sentences = Xlist.fold sentences (0,p,[]) (fun (i,p,sentences) (id_s,tokens,named_tokens) ->
    let i,p,tokens = Xlist.fold tokens (i,p,[]) (fun (i,p,tokens) (beg,len,nps,orth,lemma,cat,interp) ->
      (* Printf.printf "match_tokens: %s %n i=%d beg=%d len=%d\n" name id_p i beg len; *)
      let no_spaces,p = get_spaces 0 p in
      (* if no_spaces>0 && (nps || i=0) then Printf.printf "match_tokens spaces: %s %n i=%d beg=%d len=%d\n" name id_p i beg len; *)
      let i = i+no_spaces in
      let real_orth,p = split_front [] len p in
      let split,err = try StringMap.find err_corr (orth ^ "\t" ^ lemma ^ "\t" ^ String.concat ":" (cat :: interp)) with Not_found -> Correct,Corr in
      if beg <> i then failwith (Printf.sprintf "match_tokens 1: %s %n i=%d beg=%d len=%d" name id_p i beg len) else (
      (* if err <> Corr then Printf.printf "match_tokens err: orth=%s lemma=%s cat=%s\n" orth lemma cat; *)
      match split with
        Correct ->
          i+len, p, (beg,len,no_spaces,String.concat "" real_orth,orth,lemma,cat,interp) :: tokens
      | Single new_orth ->
          let new_orth = if err = TErr then orth else new_orth in
          i+len, p, (beg,len,no_spaces,String.concat "" real_orth,new_orth,lemma,cat,interp) :: tokens
      | Split["w",lemma1,cat1,interp1;"ogóle",lemma2,cat2,interp2] ->
          if "wogole" = String.concat "" real_orth then
            i+len, p, (beg+1,len-1,0,"ogole","ogóle",lemma2,cat2,interp2) :: (beg,1,no_spaces,"w","w",lemma1,cat1,interp1) :: tokens else
          if "wogóle" <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: wogole") else
          i+len, p, (beg+1,len-1,0,"ogóle","ogóle",lemma2,cat2,interp2) :: (beg,1,no_spaces,"w","w",lemma1,cat1,interp1) :: tokens
      | Split["z",lemma1,cat1,interp1;"pewnością",lemma2,cat2,interp2] ->
          if "spewnością" <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: spewnością") else
          i+len, p, (beg+1,len-1,0,"pewnością","pewnością",lemma2,cat2,interp2) :: (beg,1,no_spaces,"s","z",lemma1,cat1,interp1) :: tokens
      | Split["z",lemma1,cat1,interp1;"powrotem",lemma2,cat2,interp2] ->
          if "spowrotem" <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: spowrotem") else
          i+len, p, (beg+1,len-1,0,"powrotem","powrotem",lemma2,cat2,interp2) :: (beg,1,no_spaces,"s","z",lemma1,cat1,interp1) :: tokens
      | Split["Słyszała",lemma1,cat1,interp1;"m",lemma2,cat2,interp2] ->
          if "Słyszalam" <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: Słyszalam") else
          i+len, p, (beg+len-1,1,0,"m","m",lemma2,cat2,interp2) :: (beg,len-1,no_spaces,"Słyszala","Słyszała",lemma1,cat1,interp1) :: tokens
      | Split["próbowała",lemma1,cat1,interp1;"m",lemma2,cat2,interp2] ->
          if "phóbowałam" <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: próbowałam") else
          i+len, p, (beg+len-1,1,0,"m","m",lemma2,cat2,interp2) :: (beg,len-1,no_spaces,"phóbowała","próbowała",lemma1,cat1,interp1) :: tokens
      | Split["znalazła",lemma1,cat1,interp1;"m",lemma2,cat2,interp2] ->
          if "znalazłąm" <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: znalazłam") else
          i+len, p, (beg+len-1,1,0,"m","m",lemma2,cat2,interp2) :: (beg,len-1,no_spaces,"znalazłą","znalazła",lemma1,cat1,interp1) :: tokens
      | Split[orth1,lemma1,cat1,interp1;orth2,lemma2,cat2,interp2] ->
          if orth1 ^ orth2 <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 3: %s|%s <> %s" orth1 orth2 (String.concat "" real_orth)) else
          let len1 = Xlist.size (Xunicode.utf8_chars_of_utf8_string orth1) in
          let len2 = Xlist.size (Xunicode.utf8_chars_of_utf8_string orth2) in
          if len1 + len2 <> len then failwith "match_tokens 4" else
          i+len, p, (beg+len1,len2,0,orth2,orth2,lemma2,cat2,interp2) :: (beg,len1,no_spaces,orth1,orth1,lemma1,cat1,interp1) :: tokens
      | Split[orth1,lemma1,cat1,interp1;orth2,lemma2,cat2,interp2;orth3,lemma3,cat3,interp3] ->
          if orth1 ^ orth2 ^ orth3 <> String.concat "" real_orth then failwith (Printf.sprintf "match_tokens 5: %s|%s|%s <> %s" orth1 orth2 orth3 (String.concat "" real_orth)) else
          let len1 = Xlist.size (Xunicode.utf8_chars_of_utf8_string orth1) in
          let len2 = Xlist.size (Xunicode.utf8_chars_of_utf8_string orth2) in
          let len3 = Xlist.size (Xunicode.utf8_chars_of_utf8_string orth3) in
          if len1 + len2 + len3 <> len then failwith "match_tokens 6" else
          i+len, p, (beg+len1+len2,len3,0,orth3,orth3,lemma3,cat3,interp3) :: (beg+len1,len2,0,orth2,orth2,lemma2,cat2,interp2) :: (beg,len1,no_spaces,orth1,orth1,lemma1,cat1,interp1) :: tokens
      | Split _ -> failwith "match_tokens: ni")) in
    i,p,(id_s,List.rev tokens,named_tokens) :: sentences) in
  let no_spaces,p = get_spaces 0 p in
  if i+no_spaces <> len then failwith (Printf.sprintf "match_tokens 2: %s %n i=%d len=%d p='%s'" name id_p i len (String.concat "" p))
  else List.rev sentences

let rec merge_paragraphs name id_p rev = function
    ({corref=""; prefix="txt"; numbers=[id_div;id_ab]; suffix="ab"},paragraph) :: paragraphs,
    (id_div2,id_ab2,id_s,tokens,named_tokens) :: sentences ->
       (* print_endline ("B " ^ string_of_int id_p ^ " " ^ string_of_int id_p ^ " " ^ paragraph); *)
       if id_div <> id_div2 && id_ab <> id_ab2 then failwith "merge_paragraphs 1" else
       let l,sentences = merge_paragraph id_div id_ab [id_s,tokens,named_tokens] sentences in
       (* Printf.printf "%d.%d: %s\n" id_div id_ab (String.concat " " (Xlist.map l fst)); *)
       let l =
         try match_tokens name id_p paragraph l
         with e -> (print_endline (Printexc.to_string e); []) in
       merge_paragraphs name id_p ((paragraph,l) :: rev) (paragraphs,sentences)
  | [],[] -> List.rev rev
  | _ -> failwith ("merge_paragraphs 2: " ^ name ^ " " ^ string_of_int id_p)

let rec merge_entries name rev = function
    ({corref=""; prefix="txt"; numbers=[id_div]; suffix="div"},paragraphs) :: text,
    ({corref="text.xml"; prefix="txt"; numbers=[c_div]; suffix="div"},
     {corref=""; prefix="segm"; numbers=[id_segm_p]; suffix="p"},segm_sentences) :: segmentation,
    ({corref="ann_segmentation.xml"; prefix="segm"; numbers=[c_segm_p]; suffix="p"},
     {corref=""; prefix="morph"; numbers=[id_morph_p]; suffix="p"},morph_sentences) :: morphosyntax,
    ({corref="ann_morphosyntax.xml"; prefix="morph"; numbers=[c_morph_p]; suffix="p"},
     {corref=""; prefix="named"; numbers=[id_named_p]; suffix="p"},named_sentences) :: named ->
        (* print_endline ("A " ^ string_of_int id_div); *)
        if id_div <> c_div || c_div <> id_segm_p || id_segm_p <> c_segm_p ||
           c_segm_p <> id_morph_p || id_morph_p <> c_morph_p || c_morph_p <> id_named_p then failwith "merge_entries 2" else
        let sentences = merge_sentences name id_div [] (segm_sentences,morph_sentences,named_sentences) in
        let paragraphs = merge_paragraphs name id_div [] (paragraphs,sentences) in
        merge_entries name ((id_div,true,paragraphs) :: rev) (text,segmentation,morphosyntax,named)
  | ({corref=""; prefix="txt"; numbers=[id_div]; suffix="div"},paragraphs) :: text,
    ({corref="text.xml"; prefix="txt"; numbers=[c_div]; suffix="div"},
     {corref=""; prefix="segm"; numbers=[id_segm_p]; suffix="p"},segm_sentences) :: segmentation,
    ({corref="ann_segmentation.xml"; prefix="segm"; numbers=[c_segm_p]; suffix="p"},
     {corref=""; prefix="morph"; numbers=[id_morph_p]; suffix="p"},morph_sentences) :: morphosyntax, [] ->
        (* print_endline ("A " ^ string_of_int id_div); *)
        if id_div <> c_div || c_div <> id_segm_p || id_segm_p <> c_segm_p || c_segm_p <> id_morph_p then failwith "merge_entries 2" else
        let sentences = merge_sentences name id_div [] (segm_sentences,morph_sentences,[]) in
        let paragraphs = merge_paragraphs name id_div [] (paragraphs,sentences) in
        merge_entries name ((id_div,false,paragraphs) :: rev) (text,segmentation,morphosyntax,[])
  | [],[],[],[] -> List.rev rev
  | _ -> failwith "merge_entries"

let fold path s f =
  let names = get_folders path in
  Xlist.fold names s (fun s name ->
    (* print_endline name; *)
    if name = "030-2-000000012" then s else
    let typ,channel = load_header path name in
    let text = load_text path name in
    let segmentation = load_segmentation path name in
    let morphosyntax = load_morphosyntax path name in
    let named = load_named path name in
    let entries = merge_entries name [] (text,segmentation,morphosyntax,named) in
    f s (name,typ,channel,entries))

let fold_selected path name_selection typ_selection channel_selection s f =
  let names = get_folders path in
  Xlist.fold names s (fun s name ->
    if StringSet.size name_selection > 0 && not (StringSet.mem name_selection name) then s else
    (* print_endline name; *)
    if name = "030-2-000000012" then s else
    let typ,channel = load_header path name in
    if Xlist.size typ_selection > 0 && not (Xlist.mem typ_selection typ) then s else
    if Xlist.size channel_selection > 0 && not (Xlist.mem channel_selection channel) then s else
    let text = load_text path name in
    let segmentation = load_segmentation path name in
    let morphosyntax = load_morphosyntax path name in
    let named = load_named path name in
    let entries = merge_entries name [] (text,segmentation,morphosyntax,named) in
    f s (name,typ,channel,entries))

let nkjp_path = "../../NLP resources/NKJP-PodkorpusMilionowy-1.2/"

let calculate_statistics stats typ channel entries =
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      Xlist.fold sentences stats (fun stats (id_s,tokens,named_tokens) ->
        let bad_tokens = Xlist.fold tokens 0 (fun n (_,_,_,real_orth,orth,_,_,_) ->
          if real_orth = orth then n else n+1) in
        (* let all_tokens = Xlist.size tokens in *)
        let s = Printf.sprintf "%s %s %d" typ channel bad_tokens in
        StringQMap.add stats s)))

let print_stats stats =
  StringQMap.iter stats (fun k v ->
    Printf.printf "%5d %s\n" v k)

(* let _ =
  let stats = fold nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    calculate_statistics stats typ channel entries) in
  print_stats stats *)

(*
frekwencje typów:
    127 fakt
     56 inf-por
    283 konwers
      2 listy
    376 lit
      1 lit_poezja
     80 media
    175 nd
    161 net_interakt
    227 net_nieinterakt
     20 nklas
   1986 publ
      8 qmow
    387 urzed

frekwencje kanałów
    388 internet
    817 ksiazka
    363 mowiony
    146 prasa
   1744 prasa_dziennik
    398 prasa_inne
      5 prasa_miesiecznik
     28 prasa_tygodnik

frekwencje łączne typów-kanałów
    127 fakt    ksiazka
     56 inf-por ksiazka
    283 konwers mowiony
      2 listy   ksiazka
    376 lit     ksiazka
      1 lit_poezja      ksiazka
     80 media   mowiony
    175 nd      ksiazka
    161 net_interakt    internet
    227 net_nieinterakt internet
     20 nklas   ksiazka
     60 publ    ksiazka
    146 publ    prasa
   1744 publ    prasa_dziennik
      3 publ    prasa_inne
      5 publ    prasa_miesiecznik
     28 publ    prasa_tygodnik
      8 qmow    prasa_inne
    387 urzed   prasa_inne

liczba zdań z błędami w danym typie-kanale
 4164 fakt ksiazka 0
   29 fakt ksiazka 1
    2 fakt ksiazka 2
 4442 inf-por ksiazka 0
   15 inf-por ksiazka 1
 8401 konwers mowiony 0
   46 konwers mowiony 1
    1 konwers mowiony 2
   40 listy ksiazka 0
    1 listy ksiazka 1
17440 lit ksiazka 0
   66 lit ksiazka 1
    2 lit ksiazka 2
 2564 media mowiony 0
   14 media mowiony 1
 1137 nd ksiazka 0
    7 nd ksiazka 1
 4045 net_interakt internet 0
  249 net_interakt internet 1
   49 net_interakt internet 2
   15 net_interakt internet 3
    4 net_interakt internet 4
    3 net_interakt internet 5
    1 net_interakt internet 6
    1 net_interakt internet 7
 1437 net_nieinterakt internet 0
   19 net_nieinterakt internet 1
    1 net_nieinterakt internet 2
  728 nklas ksiazka 0
    5 nklas ksiazka 1
  645 publ ksiazka 0
    5 publ ksiazka 1
 8492 publ prasa 0
  140 publ prasa 1
    5 publ prasa 2
    3 publ prasa 3
19078 publ prasa_dziennik 0
  184 publ prasa_dziennik 1
    7 publ prasa_dziennik 2
  717 publ prasa_inne 0
   17 publ prasa_inne 1
 1407 publ prasa_miesiecznik 0
    4 publ prasa_miesiecznik 1
    1 publ prasa_miesiecznik 2
 6941 publ prasa_tygodnik 0
   29 publ prasa_tygodnik 1
    1 publ prasa_tygodnik 2
 2290 qmow prasa_inne 0
    6 qmow prasa_inne 1
 1380 urzed prasa_inne 0
    4 urzed prasa_inne 1

    *)
