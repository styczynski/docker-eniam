(*
 *  ENIAMtokenizer, a tokenizer for Polish
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

open Printf
open ENIAMtokenizerTypes
open Xstd
open Xunicode

let xml_of_interp interp =
  Xml.Element("interp",[],[Xml.PCData (ENIAMtagset.render [interp])])

let rec string_of_token = function
    SmallLetter(uc,lc) -> sprintf "SmallLetter(%s,%s)" uc lc
  | CapLetter(uc,lc) -> sprintf "CapLetter(%s,%s)" uc lc
  | AllSmall(uc,fc,lc) -> sprintf "AllSmall(%s,%s,%s)" uc fc lc
  | AllCap(uc,fc,lc) -> sprintf "AllCap(%s,%s,%s)" uc fc lc
  | FirstCap(uc,fc,lc) -> sprintf "FirstCap(%s,%s,%s)" uc fc lc
  | SomeCap(uc,orth,lc) -> sprintf "SomeCap(%s,%s,%s)" uc orth lc
  | RomanDig(v,t) -> sprintf "RomanDig(%s,%s)" v t
  | Interp orth -> sprintf "Interp(%s)" orth
  | Symbol orth  -> sprintf "Symbol(%s)" orth
  | Dig(v,t) -> sprintf "Dig(%s,%s)" v t
  | Other orth  -> sprintf "Other(%s)" orth
  | Lemma(lemma,cat,interps) -> sprintf "Lemma(%s,%s,%s)" lemma cat (ENIAMtagset.render interps)
(*   | Proper(lemma,cat,interps,senses) -> sprintf "Proper(%s,%s,%s,%s)" lemma cat (ENIAMtagset.render interps) (String.concat "|" senses) *)
(*  | Sense(lemma,cat,interps,senses) -> sprintf "Sense(%s,%s,%s,%s)" lemma cat (ENIAMtagset.render interps)
     (String.concat "|" (Xlist.map senses (fun (_,v,_) -> v)))*)
  | Compound(sense,l) -> sprintf "Compound(%s,[%s])" sense (String.concat ";" (Xlist.map l string_of_token))
  | Tokens(cat,l) -> sprintf "Tokens(%s,%s)" cat (String.concat ";" (Xlist.map l string_of_int))

let rec string_of_token_simple = function
    SmallLetter _ -> "SmallLetter"
  | CapLetter _ -> "CapLetter"
  | AllSmall _ -> "AllSmall"
  | AllCap _ -> "AllCap"
  | FirstCap _ -> "FirstCap"
  | SomeCap _ -> "SomeCap"
  | RomanDig(v,t) -> "RomanDig"
  | Interp orth -> sprintf "Interp(%s)" orth
  | Symbol orth  -> sprintf "Symbol(%s)" orth
  | Dig(v,t) -> "Dig"
  | Other orth  -> sprintf "Other(%s)" orth
  | Lemma(lemma,cat,interp) -> "Lemma"
(*   | Proper(lemma,cat,interp,sense) -> "Proper" *)
(*   | Sense(lemma,cat,interp,sense) -> "Sense" *)
  | Compound(sense,l) -> sprintf "Compound"
  | Tokens _ -> sprintf "Tokens"

let rec xml_of_token = function
    SmallLetter(uc,lc) -> Xml.Element("SmallLetter",["uc",uc;"lc",lc],[])
  | CapLetter(uc,lc) -> Xml.Element("CapLetter",["uc",uc;"lc",lc],[])
  | AllSmall(uc,fc,lc) -> Xml.Element("AllSmall",["uc",uc;"fc",fc;"lc",lc],[])
  | AllCap(uc,fc,lc) -> Xml.Element("AllCap",["uc",uc;"fc",fc;"lc",lc],[])
  | FirstCap(uc,fc,lc) -> Xml.Element("FirstCap",["uc",uc;"fc",fc;"lc",lc],[])
  | SomeCap(lc,orth,uc) -> Xml.Element("SomeCap",["uc",uc;"orth",orth;"lc",lc],[])
  | RomanDig(v,t) -> Xml.Element("RomanDig",["t",t],[Xml.PCData v])
  | Interp orth -> Xml.Element("Interp",[],[Xml.PCData orth])
  | Symbol orth  -> Xml.Element("Symbol",[],[Xml.PCData orth])
  | Dig(v,t) -> Xml.Element("Dig",["t",t],[Xml.PCData v])
  | Other orth  -> Xml.Element("Other",[],[Xml.PCData orth])
  | Lemma(lemma,cat,interps) -> Xml.Element("Lemma",["lemma",lemma;"pos",cat],Xlist.map interps xml_of_interp)
(*  | Proper(lemma,cat,interps,senses) -> Xml.Element("Proper",["lemma",lemma;"pos",cat],
                                                    (Xlist.map interps xml_of_interp) @
                                                    (Xlist.map senses (fun s -> Xml.Element("sense",[],[Xml.PCData s]))))*)
  | Compound(sense,l) -> Xml.Element("Compound",["sense",sense], Xlist.map l xml_of_token)
  | Tokens(cat,l) -> Xml.Element("Tokens",["pos",cat],Xlist.map l (fun x -> Xml.Element("id",[],[Xml.PCData (string_of_int x)])))

let string_of_attr = function
    FC -> "first capital"
  | CS -> "cs"
  | MaybeCS -> "maybe cs"
  | HasAglSuffix -> "has aglutinate suffix"
  | MWE -> "mwe"
  | LemmNotVal -> "lemma not validated"
  | TokNotFound -> "token not found"
  | NotValProper -> "notvalidated proper"
  | LemmLowercase -> "lemmatized as lowercase"
  | Roman -> "roman"
  | SentBeg -> "NKJP sentence begin"
  | SentEnd -> "NKJP sentence end"
  | SentBegEnd -> "NKJP sentence begin-end"
  | BrevLemma s -> "NKJP brev lemma: " ^ s
  | Disamb(lemma,cat,interp) -> "NKJP disamb: " ^ lemma ^ ":" ^ cat ^ ":" ^ String.concat ":" (Xlist.map interp (String.concat "."))
  | Capitalics -> "capitalics"

let string_of_token_env t =
  sprintf "{orth=%s;beg=%d;len=%d;next=%d;token=%s;cat=%s;weight=%.2f;lemma_frequency=%.2f;morf_frequency=%.2f;attrs=[%s];tagger_output=[%s]}" t.orth t.beg t.len t.next (string_of_token t.token) t.cat
    t.weight t.lemma_frequency t.morf_frequency
    (String.concat ";" (Xlist.map t.attrs string_of_attr))
    (String.concat ";" (Xlist.map t.tagger_output (fun (interp,prob,eos,disamb) ->
      Printf.sprintf "%s[%.4f%s%s]" interp prob (if eos then ",eos" else "") (if disamb then ",disamb" else ""))))

let xml_of_token_env id t =
  let id_attr = if id < 0 then [] else ["id",string_of_int id] in
  Xml.Element("token_record",id_attr @ ["beg",string_of_int t.beg;"len",string_of_int t.len;"next",string_of_int t.next;"weight",string_of_float t.weight],[
      Xml.Element("orth",[],[Xml.PCData t.orth]);
      xml_of_token t.token;
      Xml.Element("attrs",[],Xlist.map t.attrs (fun attr -> Xml.Element("attr",[],[Xml.PCData (string_of_attr attr)])))])

let rec spaces i =
  if i = 0 then "" else "  " ^ spaces (i-1)

let rec string_of_tokens i = function
    Token t -> sprintf "%s%s" (spaces i) (string_of_token_env t)
  | Variant l -> sprintf "%sVariant[\n%s]" (spaces i) (String.concat ";\n" (Xlist.map l (string_of_tokens (i+1))))
  | Seq l -> sprintf "%sSeq[\n%s]" (spaces i) (String.concat ";\n" (Xlist.map l (string_of_tokens (i+1))))

let rec string_of_tokens_simple = function
    Token t -> string_of_token_simple t.token
  | Variant l -> sprintf "Variant[%s]" (String.concat ";" (Xlist.map l string_of_tokens_simple))
  | Seq l -> sprintf "Seq[%s]" (String.concat ";" (Xlist.map l string_of_tokens_simple))

let rec get_orth_list = function
    Token{orth=""} -> []
  | Token t -> if Xlist.mem t.attrs HasAglSuffix then raise Not_found else [t.orth]
  | Variant l -> 
     let l = Xlist.fold l [] (fun l t -> 
       try get_orth_list t :: l with Not_found -> l) in
	 if l = [] then failwith "get_orth_list" else List.hd l
  | Seq l -> List.flatten (Xlist.map l get_orth_list)
  
let get_orth = function
    SmallLetter(uc,lc) -> lc
  | CapLetter(uc,lc) -> uc
  | AllSmall(uc,fc,lc) -> lc
  | AllCap(uc,fc,lc) -> uc
  | FirstCap(uc,fc,lc) -> fc
  | SomeCap(uc,orth,lc) -> orth
  | Symbol orth  -> orth
  | Dig(v,_) -> v
  | Other orth  -> orth
  | Interp orth  -> orth
  | _ -> ""(*failwith "get_orth"*)

let get_orths = function
    SmallLetter(uc,lc) -> [uc;lc]
  | CapLetter(uc,lc) -> [uc;lc]
  | AllSmall(uc,fc,lc) -> [uc;fc;lc]
  | AllCap(uc,fc,lc) -> [uc;fc;lc]
  | FirstCap(uc,fc,lc) -> [uc;fc;lc]
  | SomeCap(uc,orth,lc) -> [uc;orth;lc]
  | Symbol orth  -> [orth]
  | Dig(v,_) -> [v]
  | Other orth  -> [orth]
  | Interp orth  -> [orth]
  | _ -> []

let rec get_lemma = function
    Interp orth -> orth
  | Lemma(lemma,cat,_) -> lemma
(*   | Proper(lemma,cat,_,_) -> lemma *)
  | _ -> ""

let rec get_pos = function
    Interp _ -> "interp"
  | Lemma(lemma,cat,_) -> cat
(*   | Proper(lemma,cat,_,_) -> cat *)
  | _ -> ""

let rec get_interp = function
    Interp orth -> ""
  | Lemma(lemma,cat,interp) -> ENIAMtagset.render interp
(*   | Proper(lemma,cat,interp,_) -> ENIAMtagset.render interp *)
  | _ -> ""

let months = StringSet.of_list ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09"; "10"; "11"; "12"]
let hours = StringSet.of_list ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "00"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09";
   "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24"]
let days = StringSet.of_list ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "01"; "02"; "03"; "04"; "05"; "06"; "07"; "08"; "09";
   "10"; "11"; "12"; "13"; "14"; "15"; "16"; "17"; "18"; "19"; "20"; "21"; "22"; "23"; "24"; "25"; "26"; "27"; "28"; "29"; "30"; "31"]
let romanmonths = StringSet.of_list ["I"; "II"; "III"; "IV"; "V"; "VI"; "VII"; "VIII"; "IX"; "X"; "XI"; "XII"]

let dig_token orth i digs token =
  Token{empty_token_env with orth=orth;beg=i;len=Xlist.size digs * factor;next=i+Xlist.size digs * factor;
    token=token; attrs=[MaybeCS]}

(* let sc_dig_token orth i digs token =
  Seq[s_beg i;c_beg (i+1);Token{empty_token_env with orth=orth;beg=i+2;len=Xlist.size digs * factor - 2;next=i+Xlist.size digs * factor; token=token; attrs=[MaybeCS]}] *)

let dig_tokens orth i digs v cat =
  [Token{empty_token_env with orth=orth;beg=i;len=Xlist.size digs * factor;next=i+Xlist.size digs * factor;
    token=Dig(v,cat); attrs=[MaybeCS]}]

let merge_digits i digs =
  let orth = String.concat "" digs in
  let t = dig_tokens orth i digs in
  let v = try Printf.sprintf "%02d" (int_of_string orth) with _ -> failwith "merge_digits" in
  let variants =
    (t orth "dig") @
    [Token{empty_token_env with orth=orth;beg=i;len=Xlist.size digs * factor;next=i+Xlist.size digs * factor; token=Lemma(*Proper*)(orth,"obj-id",[[]](*,["obj-id"]*)); attrs=[MaybeCS]}] @
    (if List.hd digs <> "0" then [Token{empty_token_env with orth=orth;beg=i;len=Xlist.size digs * factor;next=i+Xlist.size digs * factor; token=Lemma(*Proper*)(orth,"building-number",[[]](*,["building-number"]*)); attrs=[MaybeCS]}] else []) @
    (if digs = ["0"] || List.hd digs <> "0" then (t orth "intnum")(* @ (t orth "realnum")*) else []) @
    (if List.hd digs <> "0" then (t orth "year") else []) @
    (if StringSet.mem months orth then (t v "month") else []) @
    (if StringSet.mem hours orth then (t v "hour") else []) @
    (if StringSet.mem days orth then (t v "day") else []) @
    (if Xlist.size digs = 2 && List.hd digs < "6" then (t v "minute") else []) @
    (if Xlist.size digs = 9 then [dig_token orth i digs (Lemma(*Proper*)(orth,"phone-number",[[]](*,["phone-number"]*)))] else []) @
    (if Xlist.size digs = 4 then (t orth "4dig") else []) @
    (if Xlist.size digs = 3 then (t orth "3dig") else []) @
    (if Xlist.size digs = 2 then (t orth "2dig") else []) @
    (if Xlist.size digs <= 3 && List.hd digs <> "0" then (t orth "pref3dig") else []) in
  Variant variants

let recognize_roman_I v = function
    Capital("I",_) :: Capital("I",_) :: Capital("I",_) :: [] -> v+3,false
  | Capital("I",_) :: Capital("I",_) :: [] -> v+2,false
  | Capital("I",_) :: [] -> v+1,false
  | [] -> v,false
  | Capital("I",_) :: Capital("I",_) :: Capital("I",_) :: Small(_,"w") :: [] -> v+3,true
  | Capital("I",_) :: Capital("I",_) :: Small(_,"w") :: [] -> v+2,true
  | Capital("I",_) :: Small(_,"w") :: [] -> v+1,true
  | Small(_,"w") :: [] -> v,true
  | _ -> 0,false

let recognize_roman_V v = function
    Capital("I",_) :: ForeignCapital("V",_) :: [] -> v+4,false
  | ForeignCapital("V",_) :: l -> recognize_roman_I (v+5) l
  | Capital("I",_) :: ForeignCapital("X",_) :: [] -> v+9,false
  | Capital("I",_) :: ForeignCapital("V",_) :: Small(_,"w") :: [] -> v+4,true
  | Capital("I",_) :: ForeignCapital("X",_) :: Small(_,"w") :: [] -> v+9,true
  | l -> recognize_roman_I v l

let recognize_roman_X v = function
  | ForeignCapital("X",_) :: ForeignCapital("X",_) :: ForeignCapital("X",_) :: l -> recognize_roman_V (v+30) l
  | ForeignCapital("X",_) :: ForeignCapital("X",_) :: l -> recognize_roman_V (v+20) l
  | ForeignCapital("X",_) :: l -> recognize_roman_V (v+10) l
  | l -> recognize_roman_V v l

let recognize_roman_L v = function
    ForeignCapital("X",_) :: Capital("L",_) :: l -> recognize_roman_V (v+40) l
  | Capital("L",_) :: l -> recognize_roman_X (v+50) l
  | ForeignCapital("X",_) :: Capital("C",_) :: l -> recognize_roman_V (v+90) l
  | l -> recognize_roman_X v l

let recognize_roman_C v = function
  | Capital("C",_) :: Capital("C",_) :: Capital("C",_) :: l -> recognize_roman_L (v+300) l
  | Capital("C",_) :: Capital("C",_) :: l -> recognize_roman_L (v+200) l
  | Capital("C",_) :: l -> recognize_roman_L (v+100) l
  | l -> recognize_roman_L v l

let recognize_roman_D v = function
    Capital("C",_) :: Capital("D",_) :: l -> recognize_roman_L (v+400) l
  | Capital("D",_) :: l -> recognize_roman_C (v+500) l
  | Capital("C",_) :: Capital("M",_) :: l -> recognize_roman_L (v+900) l
  | l -> recognize_roman_C v l

let recognize_roman_M v = function
  | Capital("M",_) :: Capital("M",_) :: Capital("M",_) :: l -> recognize_roman_D (v+3000) l
  | Capital("M",_) :: Capital("M",_) :: l -> recognize_roman_D (v+2000) l
  | Capital("M",_) :: l -> recognize_roman_D (v+1000) l
  | l -> recognize_roman_D v l

let rec merge l =
  String.concat "" (Xlist.map l (function
      Capital(uc,lc) -> uc
    | ForeignCapital(uc,lc) -> uc
    | Small(uc,lc) -> lc
    | ForeignSmall(uc,lc) -> lc
    | _ -> failwith "merge"))

let lowercase_first = function
    [] -> []
  | Capital(uc,lc) :: l -> Small(uc,lc) :: l
  | ForeignCapital(uc,lc) :: l -> ForeignSmall(uc,lc) :: l
  | Small(uc,lc) :: l -> Small(uc,lc) :: l
  | ForeignSmall(uc,lc) :: l -> ForeignSmall(uc,lc) :: l
  | _ -> failwith "lowercase_first"

let rec lowercase_all = function
    [] -> []
  | Capital(uc,lc) :: l -> Small(uc,lc) :: lowercase_all l
  | ForeignCapital(uc,lc) :: l -> ForeignSmall(uc,lc) :: lowercase_all l
  | Small(uc,lc) :: l -> Small(uc,lc) :: lowercase_all l
  | ForeignSmall(uc,lc) :: l -> ForeignSmall(uc,lc) :: lowercase_all l
  | _ -> failwith "lowercase_all"

let rec uppercase_all = function
    [] -> []
  | Capital(uc,lc) :: l -> Capital(uc,lc) :: uppercase_all l
  | ForeignCapital(uc,lc) :: l -> ForeignCapital(uc,lc) :: uppercase_all l
  | Small(uc,lc) :: l -> Capital(uc,lc) :: uppercase_all l
  | ForeignSmall(uc,lc) :: l -> ForeignCapital(uc,lc) :: uppercase_all l
  | _ -> failwith "uppercase_all"

let make_first_cap = function
    [] -> []
  | Capital(uc,lc) :: l -> Capital(uc,lc) :: lowercase_all l
  | ForeignCapital(uc,lc) :: l -> ForeignCapital(uc,lc) :: lowercase_all l
  | Small(uc,lc) :: l -> Capital(uc,lc) :: lowercase_all l
  | ForeignSmall(uc,lc) :: l -> ForeignCapital(uc,lc) :: lowercase_all l
  | _ -> failwith "make_first_cap"

(*let lowercase_rest = function
    [] -> []
  | x :: l -> x :: lowercase_all l

let first_capital = function
    Capital _ :: _ -> true
  | ForeignCapital _ :: _ -> true
  | Small _ :: _ -> false
  | ForeignSmall _ :: _ -> false
  | _ -> failwith "first_capital"

let rec all_capital = function
    Capital _ :: l -> all_capital l
  | ForeignCapital _ :: l -> all_capital l
  | Small _ :: l -> false
  | ForeignSmall _ :: l -> false
  | [] -> true
  | _ -> failwith "first_capital"

let rec all_small = function
    Capital _ :: l -> false
  | ForeignCapital _ :: l -> false
  | Small _ :: l -> all_small l
  | ForeignSmall _ :: l -> all_small l
  | [] -> true
  | _ -> failwith "first_capital"

let rest_capital = function
    [] -> failwith "rest_capital"
  | _ :: l -> all_capital l

let rest_small = function
    [] -> failwith "rest_small"
  | _ :: l -> all_small l

let get_first_cap = function
  | Capital(s,t) :: l -> s
  | ForeignCapital(s,t) :: l -> s
  | _ -> failwith "get_first_cap"

let get_first_lower = function
  | Capital(s,t) :: l -> t
  | ForeignCapital(s,t) :: l -> t
  | _ -> failwith "get_first_lower"*)

(*let cs_weight = -1.
let fc_weight = -10.
let sc_cap_weight = -0.3*)

(*let is_add_attr_token = function
    SmallLetter _ -> true
  | CapLetter _ -> true
  | AllSmall _ -> true
  | AllCap _ -> true
  | FirstCap _ -> true
  | SomeCap _ -> true
  | _ -> false

let rec add_attr s = function
    Token t -> if is_add_attr_token t.token then Token{t with attrs=s :: t.attrs} else Token t
  | Variant l -> Variant(Xlist.map l (add_attr s))
  | Seq l -> Seq(Xlist.map l (add_attr s))*)

let recognize_stem has_sufix i letters =
  let orth = merge letters in
  let lowercase = merge (lowercase_all letters) in
  let uppercase = merge (uppercase_all letters) in
  let first_cap = merge (make_first_cap letters) in
  let token = if Xlist.size letters = 1 then
    if orth = lowercase then SmallLetter(uppercase,lowercase) else
    if orth = uppercase then CapLetter(uppercase,lowercase) else
    failwith "recognize_stem"
  else
    if orth = lowercase then AllSmall(uppercase,first_cap,lowercase) else
    if orth = uppercase then AllCap(uppercase,first_cap,lowercase) else
    if orth = first_cap then FirstCap(uppercase,first_cap,lowercase) else
    SomeCap(uppercase,orth,lowercase) in
  Token{empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor; token=token;
    attrs=if has_sufix then [HasAglSuffix] else []}
(*  let orth = merge letters in
  let t = {empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor} in
  let t = if poss_s_beg then
    if Xlist.size letters = 1 then
      if first_capital letters then Variant[
          Token{t with token=SmallLetter(merge (lowercase_first letters)); weight=cs_weight; attrs=CS :: t.attrs};
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=SmallLetter(merge (lowercase_first letters)); attrs=MaybeCS :: t.attrs}];
          Token{t with token=CapLetter(orth,merge (lowercase_first letters)); attrs=MaybeCS :: t.attrs};
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=CapLetter(orth,merge (lowercase_first letters)); weight=sc_cap_weight; attrs=MaybeCS :: t.attrs}]]
      else if !internet_mode then Variant[
          Token{t with token=SmallLetter orth};
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=SmallLetter orth}]]
      else Token{t with token=SmallLetter orth}
    else
      if first_capital letters then
        if rest_small letters then Variant([
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=AllSmall(merge (lowercase_first letters))}];
          Token{t with token=FirstCap(orth,merge (lowercase_first letters),get_first_cap letters,get_first_lower letters)};
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=FirstCap(orth,merge (lowercase_first letters),get_first_cap letters,get_first_lower letters); weight=sc_cap_weight}]] @
          (if !internet_mode then [Token{t with token=AllSmall(merge (lowercase_first letters)); weight=fc_weight; attrs=FC :: t.attrs}] else []))
        else if rest_capital letters then Variant([
          Token{t with token=AllSmall(merge (lowercase_all letters)); weight=cs_weight; attrs=CS :: t.attrs};
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=AllSmall(merge (lowercase_all letters)); weight=cs_weight; attrs=CS :: t.attrs}];
          Token{t with token=FirstCap(merge (lowercase_rest letters),merge (lowercase_all letters),get_first_cap letters,get_first_lower letters); weight=cs_weight; attrs=CS :: t.attrs};
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=FirstCap(merge (lowercase_rest letters),merge (lowercase_all letters),get_first_cap letters,get_first_lower letters); weight=cs_weight+.sc_cap_weight; attrs=CS :: t.attrs}]] @
          (if has_sufix then [] else [
           Token{t with token=AllCap(orth,merge (lowercase_rest letters),merge (lowercase_all letters)); attrs=MaybeCS :: t.attrs};
           Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=AllCap(orth,merge (lowercase_rest letters),merge (lowercase_all letters)); attrs=MaybeCS :: t.attrs}]]))
        else Token{t with token=SomeCap orth}
      else if !internet_mode then
        if rest_small letters then Variant[
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=AllSmall orth}];
          Token{t with token=AllSmall orth}]
        else Variant[
          Seq[s_beg i;c_beg (i+1);Token{t with beg=t.beg+2; len=t.len-2; token=SomeCap orth}];
          Token{t with token=SomeCap orth}]
      else
        if rest_small letters then Token{t with token=AllSmall orth}
        else Token{t with token=SomeCap orth}
  else
    if Xlist.size letters = 1 then
      if first_capital letters then Variant[
          Token{t with token=SmallLetter(merge (lowercase_first letters)); weight=cs_weight; attrs=CS :: t.attrs};
          Token{t with token=CapLetter(orth,merge (lowercase_first letters)); attrs=MaybeCS :: t.attrs}]
      else Token{t with token=SmallLetter orth}
    else
      if first_capital letters then
        if rest_small letters then Variant([
          Token{t with token=FirstCap(orth,merge (lowercase_first letters),get_first_cap letters,get_first_lower letters)}] @
          (if !internet_mode then [Token{t with token=AllSmall(merge (lowercase_first letters)); weight=fc_weight; attrs=FC :: t.attrs}] else []))
        else if rest_capital letters then Variant([
          Token{t with token=AllSmall(merge (lowercase_all letters)); weight=cs_weight; attrs=CS :: t.attrs};
          Token{t with token=FirstCap(merge (lowercase_rest letters),merge (lowercase_all letters),get_first_cap letters,get_first_lower letters); weight=cs_weight; attrs=CS :: t.attrs}] @
          (if has_sufix then [] else [
           Token{t with token=AllCap(orth,merge (lowercase_rest letters),merge (lowercase_all letters)); attrs=MaybeCS :: t.attrs}]))
        else Token{t with token=SomeCap orth}
      else
        if rest_small letters then Token{t with token=AllSmall orth}
        else Token{t with token=SomeCap orth} in
  if has_sufix then add_attr HasAglSuffix t else t*)

let make_lemma (lemma,interp) =
  match ENIAMtagset.parse_and_validate lemma interp with
    [pos,tags] -> Lemma(lemma,pos,tags)
  | [pos1,tags1;pos2,tags2] -> if pos1=pos2 then Lemma(lemma,pos1,tags1 @ tags2) else failwith "make_lemma 1"
  | _ -> failwith "make_lemma 2"

let merge_attrs l =
(*   print_endline (String.concat " " (Xlist.map l (fun token -> "[" ^ token.orth ^ " " ^ String.concat ";" token.attrs ^ "]"))); *)
  let len = Xlist.size l in
  let attrs = Xlist.fold l AttrQMap.empty (fun attrs token ->
    Xlist.fold token.attrs attrs AttrQMap.add) in
  let n_cs = try AttrQMap.find attrs CS with Not_found -> 0 in
  let n_maybe_cs = try AttrQMap.find attrs MaybeCS with Not_found -> 0 in
  let new_attrs =
    (if n_cs > 0 then
      if n_cs + n_maybe_cs = len then [CS] else raise Not_found
    else
      if n_maybe_cs = len then [MaybeCS] else []) @
    (AttrQMap.fold attrs [] (fun attrs attr _ -> if attr = CS || attr = MaybeCS then attrs else attr :: attrs)) in
(*   print_endline (String.concat " " new_attrs); *)
  new_attrs

let suffix_lemmata = Xlist.fold [
  "em",make_lemma ("być","aglt:sg:pri:imperf:wok");
  "eś",make_lemma ("być","aglt:sg:sec:imperf:wok");
  "eście",make_lemma ("być","aglt:pl:sec:imperf:wok");
  "eśmy",make_lemma ("być","aglt:pl:pri:imperf:wok");
  "m",make_lemma ("być","aglt:sg:pri:imperf:nwok");
  "ś",make_lemma ("być","aglt:sg:sec:imperf:nwok");
  "ście",make_lemma ("być","aglt:pl:sec:imperf:nwok");
  "śmy",make_lemma ("być","aglt:pl:pri:imperf:nwok");
  "by",make_lemma ("by","qub");
  "ń",make_lemma ("on","ppron3:sg:gen.acc:m1.m2.m3:ter:nakc:praep");
  "że",make_lemma ("że","qub");
  "ż",make_lemma ("że","qub");
  ] StringMap.empty (fun map (suf,lemma) -> StringMap.add map suf lemma)

let recognize_suffix i letters =
  let orth = merge (lowercase_all letters) in (* FIXME: brak informacji o zmianie wielkości liter *)
  let t = {empty_token_env with orth=merge letters;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor} in
  Token{t with token=StringMap.find suffix_lemmata orth}


(*let recognize_suffix i letters =
  let orth = merge letters in
  let lowercase = merge (lowercase_all letters) in
  let uppercase = merge (uppercase_all letters) in
  let first_cap = merge (make_first_cap letters) in
  let token = if Xlist.size letters = 1 then
    if orth = lowercase then SmallLetter(uppercase,lowercase) else
    if orth = uppercase then CapLetter(uppercase,lowercase) else
    failwith "recognize_stem"
  else
    if orth = lowercase then AllSmall(uppercase,first_cap,lowercase) else
    if orth = uppercase then AllCapital(uppercase,first_cap,lowercase) else
    if orth = first_cap then FirstCap(uppercase,first_cap,lowercase) else
    SomeCap(uppercase,orth,lowercase) in
  Token{empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor; token=token}

  let t = {empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor} in
  if all_capital letters then Token{t with token=StringMap.find suffix_lemmata (merge (lowercase_all letters)); weight=cs_weight; attrs=CS :: t.attrs}
  else if all_small letters then Token{t with token=StringMap.find suffix_lemmata orth}
  else raise Not_found*)

let recognize_romandig i letters =
  let roman,w = recognize_roman_M 0 letters in
  if roman > 0 then
    let letters,w = if w then let l = List.rev letters in List.rev (List.tl l), [List.hd l] else letters,[] in
    let orth = merge letters in
    let roman = string_of_int roman in
    let t = {empty_token_env with orth=orth;beg=i;len=Xlist.size letters * factor;next=i+Xlist.size letters * factor} in
    let w = if w = [] then [] else
      let beg = i + Xlist.size letters * factor in
      [Variant[Token{empty_token_env with orth=merge w; beg=beg; len=factor; next=beg+factor; token=SmallLetter(merge (uppercase_all w),merge w)};
               Token{empty_token_env with orth=merge w; beg=beg; len=factor; next=beg+factor; token=make_lemma ("wiek","subst:sg:_:m3")}]] in
    if StringSet.mem romanmonths orth then [
      Seq(Token{t with token=RomanDig(roman,"roman"); attrs=MaybeCS :: t.attrs}::w);
      Seq(Token{t with token=RomanDig(roman,"month"); attrs=MaybeCS :: t.attrs}::w)]
    else [
      Seq(Token{t with token=RomanDig(roman,"roman"); attrs=MaybeCS :: t.attrs}::w)]
  else []

let sufixes1 = Xlist.map [
  ["m"];
  ["e"; "m"];
  ["ś"];
  ["e"; "ś"];
  ["ś"; "m"; "y"];
  ["e"; "ś"; "m"; "y"];
  ["ś"; "c"; "i"; "e"];
  ["e"; "ś"; "c"; "i"; "e"];
  ["ń"];
  ["ż"; "e"];
  ["ż"];
  ] List.rev

let sufixes2 = Xlist.map [
  ["b"; "y"];
  ] List.rev

let rec find_suffix rev = function
    _, [] -> raise Not_found
  | [], l -> rev, l
  | s :: pat, Capital(uc,lc) :: l -> if s = lc then find_suffix (Capital(uc,lc) :: rev) (pat,l) else raise Not_found
  | s :: pat, Small(uc,lc) :: l -> if s = lc then find_suffix (Small(uc,lc) :: rev) (pat,l) else raise Not_found
  | _,_ -> raise Not_found

let find_suffixes2 sufixes letters sufs =
  Xlist.fold sufixes [] (fun l suf ->
    try
      let suf,rev_stem = find_suffix [] (suf,letters) in
      (rev_stem,suf :: sufs) :: l
    with Not_found -> l)

let find_suffixes i letters =
  let letters = List.rev letters in
  let l = (letters,[]) :: find_suffixes2 sufixes1 letters [] in
  let l = Xlist.fold l l (fun l (letters,sufs) ->
    (find_suffixes2 sufixes2 letters sufs) @ l) in
  Xlist.map l (fun (rev_stem, sufs) ->
    List.rev (fst (Xlist.fold (List.rev rev_stem :: sufs) ([],i) (fun (seq,i) letters ->
      (letters,i) :: seq, i + factor * Xlist.size letters))))

let merge_letters i letters =
  let l = find_suffixes i letters in
  let roman = recognize_romandig i letters in
  let variants = Xlist.fold l roman (fun variants -> function
      [] -> failwith "merge_letters"
    | [stem,i] -> (recognize_stem false i stem) :: variants
    | (stem,i) :: suffixes ->
        (try (Seq((recognize_stem true i stem) :: Xlist.map suffixes (fun (suf,i) -> recognize_suffix i suf))) :: variants
         with Not_found -> variants)) in
  Variant variants

let rec group_digits rev = function
    [] -> List.rev rev, []
  | Digit s :: l -> group_digits (s :: rev) l
  | x :: l -> List.rev rev, x :: l

let rec group_letters rev = function
    [] -> List.rev rev, []
  | Capital(uc,lc) :: l -> group_letters ((Capital(uc,lc)) :: rev) l
  | ForeignCapital(uc,lc) :: l -> group_letters ((ForeignCapital(uc,lc)) :: rev) l
  | Small(uc,lc) :: l -> group_letters ((Small(uc,lc)) :: rev) l
  | ForeignSmall(uc,lc) :: l -> group_letters ((ForeignSmall(uc,lc)) :: rev) l
  | x :: l -> List.rev rev, x :: l

let rec group_others rev = function
    [] -> List.rev rev, []
  | Other(s,_) :: l -> group_others (s :: rev) l
  | x :: l -> List.rev rev, x :: l

let create_sign_token i signs l token =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | Small(uc,lc) -> lc | _ -> failwith "create_sign_token")) in
  let len = Xlist.size signs * factor in
  Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=token; attrs=[]},i+len,l

(*let create_empty_sign_token i signs =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_empty_sign_token")) in
  let len = Xlist.size signs * factor in
  {empty_token_env with orth=orth;beg=i;len=len;next=i+len; attrs=[MaybeCS]},i+len

let create_sentence_seq i signs l lemma =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_sentence_seq")) in
  let len = Xlist.size signs * factor in
  Seq[Token{empty_token_env with beg=i;len=20;next=i+20;token=Interp "</clause>"};
      Token{empty_token_env with orth=orth;beg=i+20;len=len-30;next=i+len-10;token=make_lemma (lemma,"sinterj")};
      Token{empty_token_env with beg=i+len-10;len=10;next=i+len;token=Interp "</sentence>"}]

let create_sentence_seq_hapl i signs l lemma =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_sentence_seq_hapl")) in
  let len = Xlist.size signs * factor in
  Seq[Token{empty_token_env with beg=i;len=10;next=i+10;token=Symbol "."; attrs=[MaybeCS]};
      Token{empty_token_env with beg=i+10;len=10;next=i+20;token=Interp "</clause>"};
      Token{empty_token_env with orth=orth;beg=i+20;len=len-30;next=i+len-10;token=make_lemma (lemma,"sinterj")};
      Token{empty_token_env with beg=i+len-10;len=10;next=i+len;token=Interp "</sentence>"}]

let create_sentence_seq_q i signs l lemma =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_sentence_seq_q")) in
  let len = Xlist.size signs * factor in
  Seq[Token{empty_token_env with beg=i;len=20;next=i+20;token=Interp "?"};
      Token{empty_token_env with beg=i+20;len=10;next=i+30;token=Interp "</clause>"};
      Token{empty_token_env with orth=orth;beg=i+30;len=len-40;next=i+len-10;token=make_lemma (lemma,"sinterj")};
      Token{empty_token_env with beg=i+len-10;len=10;next=i+len;token=Interp "</sentence>"}]

let create_sentence_seq_hapl_q i signs l lemma =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_sentence_seq_hapl_q")) in
  let len = Xlist.size signs * factor in
  Seq[Token{empty_token_env with beg=i;len=10;next=i+10;token=Symbol "."; attrs=[MaybeCS]};
      Token{empty_token_env with beg=i+10;len=10;next=i+20;token=Interp "?"};
      Token{empty_token_env with beg=i+20;len=10;next=i+30;token=Interp "</clause>"};
      Token{empty_token_env with orth=orth;beg=i+30;len=len-40;next=i+len-10;token=make_lemma (lemma,"sinterj")};
      Token{empty_token_env with beg=i+len-10;len=10;next=i+len;token=Interp "</sentence>"}]*)

let create_or_beg i signs l =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_or_beg")) in
  let len = Xlist.size signs * factor in
  Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "-"; attrs=[]},i+len,l
(*  Variant([
    Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Symbol "-"; attrs=[]};
    Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "-"; attrs=[]}]),i+len,l*)

let create_or_beg2 i signs l =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_or_beg2")) in
  let len = Xlist.size signs * factor in
  Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "-"; attrs=[]},i+len,l

(*let is_dot_sentence_end_marker = function
    [] -> true
  | [Sign " "] -> true
  | [Sign "﻿"] -> true
  | [Sign " "] -> true
  | [Sign "\""] -> true
  | [Sign "»"] -> true
  | [Sign "”"] -> true
  | _ -> false

let not_dot_sentence_end_marker = function
    Sign " " :: Small "p" :: Capital("H","h") :: _ -> false
  | Sign "﻿" :: Small "p" :: Capital("H","h") :: _ -> false
  | Sign " " :: Small "p" :: Capital("H","h") :: _ -> false
  | Sign " " :: Small "p" :: Small "h" :: _ -> false
  | Sign "﻿" :: Small "p" :: Small "h" :: _ -> false
  | Sign " " :: Small "p" :: Small "h" :: _ -> false
  | Sign " " :: Small _ :: _ -> if !internet_mode then false else true
  | Sign "﻿" :: Small _ :: _ -> if !internet_mode then false else true
  | Sign " " :: Small _ :: _ -> if !internet_mode then false else true
  | Sign "," :: _ -> true
  | Sign ":" :: _ -> true
  | Sign "?" :: _ -> true
  | Sign "!" :: _ -> true
  | Small "p" :: Capital("H","h") :: _ -> false
  | Small "p" :: Small "h" :: _ -> false
  | Small _ :: _ -> if !internet_mode then false else true
  | ForeignSmall _ :: _ -> if !internet_mode then false else true
  | Capital _ :: _ -> if !internet_mode then false else true
  | ForeignCapital _ :: _ -> if !internet_mode then false else true
  | Digit _ :: _ -> if !internet_mode then false else true
  | _ -> false

let is_comma_digit_marker = function
    Digit _ :: l -> true
  | _ -> false

let is_colon_sentence_end_marker = function
    [] -> true
  | [Sign " "] -> true
  | [Sign "﻿"] -> true
  | [Sign " "] -> true
  | _ -> false

let is_colon_symbol = function
    Digit _ :: _ -> true
  | Sign "/" :: _ -> true
  | _ -> false

let is_multidot_sentence_end_marker = function
    [] -> true
  | [Sign " "] -> true
  | [Sign "﻿"] -> true
  | [Sign " "] -> true
  | [Sign "\""] -> true
  | [Sign "»"] -> true
  | [Sign "”"] -> true
(*  | "\"" :: l -> true
  | "»" :: l -> true
  | "”" :: l -> true
  | "“" :: l -> true
  | " " :: "-" :: l -> true
  | " " :: "–" :: l -> true
  | " " :: "—" :: l -> true
  | ")" :: l -> true
  | "]" :: l -> true*)
  | _ -> false

let create_quot_digit_token i signs l =
  let t,i2 = create_empty_sign_token i signs in
  Variant[
    Seq[Token{empty_token_env with beg=i;len=20;next=i+20;token=Interp "</clause>"};
        Token{empty_token_env with orth=".";beg=i+20;len=factor-20;next=i+factor;token=Interp "</sentence>"};
        Token{t with beg=t.beg+factor; next=t.next+factor;token=Interp "”s"}];
    Seq[Token{t with token=Interp "”"};
        Token{empty_token_env with beg=i2;len=20;next=i2+20;token=Interp "</clause>"};
        Token{empty_token_env with orth=".";beg=i2+20;len=factor-20;next=i2+factor;token=Interp "</sentence>"}];
    ],i2+factor,l,true

let rec get_sign signs sign rev = function
    Sign s :: l -> if s = sign then get_sign signs sign (Sign s :: rev) l else signs @ List.rev rev, Sign s :: l
  | x :: l -> signs @ List.rev rev, x :: l
  | [] -> signs @ List.rev rev, []

let create_sign_token_rep poss_s_beg i signs sign l =
  let signs,l = get_sign signs sign [] l in
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_sign_token_rep")) in
  create_sign_token poss_s_beg i signs l (make_lemma (orth,"sinterj"))*)

(*let rec recognize_sign_group poss_s_beg i = function
  | (Sign "\"") :: (Sign ".") :: l -> create_quot_digit_token i [Sign "\""] l
  | (Sign "\"") :: l ->
      let t,i2 = create_empty_sign_token i [Sign "\""] in
      Variant[sc_dig_token "\"" i [Sign "\""] (Interp "„x"); Token{t with token=Interp "„"};Token{t with token=Interp "„s"};Token{t with token=Interp "”"};Token{t with token=Interp "”s"}],i2,l,poss_s_beg
  | (Sign "˝") :: (Sign ".") :: l -> create_quot_digit_token i [Sign "˝"] l
  | (Sign "˝") :: l ->
      let t,i2 = create_empty_sign_token i [Sign "˝"] in
      Variant[sc_dig_token "˝" i [Sign "˝"] (Interp "„x"); Token{t with token=Interp "„"};Token{t with token=Interp "„s"};Token{t with token=Interp "”"};Token{t with token=Interp "”s"}],i2,l,poss_s_beg
  | (Sign "„") :: l ->
      let t,i2 = create_empty_sign_token i [Sign "„"] in
      Variant[sc_dig_token "„" i [Sign "„"] (Interp "„x"); Token{t with token=Interp "„"};Token{t with token=Interp "„s"}],i2,l,poss_s_beg
  | (Sign "”") :: (Sign ".") :: l -> create_quot_digit_token i [Sign "”"] l
  | (Sign "”") :: l ->
      let t,i = create_empty_sign_token i [Sign "”"] in
      Variant[Token{t with token=Interp "”"};Token{t with token=Interp "”s"}],i,l,poss_s_beg
  | (Sign "“") :: (Sign ".") :: l -> create_quot_digit_token i [Sign "“"] l
  | (Sign "“") :: l ->
      let t,i2 = create_empty_sign_token i [Sign "“"] in
      Variant[sc_dig_token "“" i [Sign "“"] (Interp "„x"); Token{t with token=Interp "„"};Token{t with token=Interp "„s"};Token{t with token=Interp "”"};Token{t with token=Interp "”s"}],i2,l,poss_s_beg
  | (Sign ",") :: (Sign ",") :: l ->
      let t,i2 = create_empty_sign_token i [Sign ",";Sign ","] in
      Variant[sc_dig_token ",," i [Sign ",";Sign ","] (Interp "„x"); Token{t with token=Interp "„"};Token{t with token=Interp "„s"}],i2,l,poss_s_beg
  | (Sign "(") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ")") :: []) l (make_lemma ("(…)","sinterj"))
  | (Sign "(") :: (Sign "?") :: (Sign "!") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "?") :: (Sign "!") :: (Sign ")") :: []) l (make_lemma ("(?!)","sinterj"))
  | (Sign "(") :: (Sign ".") :: (Sign ".") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign ".") :: (Sign ".") :: (Sign ")") :: []) l (make_lemma ("(…)","sinterj"))
  | (Sign "(") :: (Sign "+") :: (Sign "+") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "+") :: (Sign "+") :: (Sign ")") :: []) l (make_lemma ("(++)","sinterj"))
  | (Sign "(") :: (Sign "-") :: (Sign "-") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "-") :: (Sign "-") :: (Sign ")") :: []) l (make_lemma ("(--)","symbol"))
  | (Sign "(") :: (Sign "…") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "…") :: (Sign ")") :: []) l (make_lemma ("(…)","sinterj"))
  | (Sign "(") :: (Sign "?") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "?") :: (Sign ")") :: []) l (make_lemma ("(?)","sinterj"))
  | (Sign "(") :: (Sign "+") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "+") :: (Sign ")") :: []) l (make_lemma ("(+)","symbol"))
  | (Sign "(") :: (Sign "!") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "!") :: (Sign ")") :: []) l (make_lemma ("(!)","sinterj"))
  | (Sign "(") :: (Sign "-") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "-") :: (Sign ")") :: []) l (make_lemma ("(-)","symbol"))
  | (Sign "(") :: (Sign "*") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign "(") :: (Sign "*") :: (Sign ")") :: []) l (make_lemma ("(*)","symbol"))
  | (Sign ":") :: (Sign "(") :: l -> create_sign_token_rep poss_s_beg i ((Sign ":") :: (Sign "(") :: []) "(" l
  | (Sign ":") :: (Sign "-") :: (Sign "(") :: l -> create_sign_token_rep poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign "(") :: []) "(" l
  | (Sign ";") :: (Sign "(")  :: l -> create_sign_token_rep poss_s_beg i ((Sign ";") :: (Sign "(") :: []) "(" l
  | (Sign ";") :: (Sign "-") :: (Sign "(") :: l -> create_sign_token_rep poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign "(") :: []) "(" l
  (* | (Sign ":") :: (Sign "(") :: (Sign "(") :: (Sign "(") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "(") :: (Sign "(") :: (Sign "(") :: []) l (make_lemma (":(((","sinterj")) *)
  (* | (Sign ":") :: (Sign "(") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "(") :: []) l (make_lemma (":(","sinterj")) *)
  (* | (Sign ":") :: (Sign "-") :: (Sign "(") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign "(") :: []) l (make_lemma (":-(","sinterj")) *)
  (* | (Sign ";") :: (Sign "(") :: (Sign "(") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "(") :: (Sign "(") :: []) l (make_lemma (";((","sinterj")) *)
  | (Sign ":") :: (Sign ")") :: l -> create_sign_token_rep poss_s_beg i ((Sign ":") :: (Sign ")") :: []) ")" l
  | (Sign ":") :: (Sign "-") :: (Sign ")") :: l -> create_sign_token_rep poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign ")") :: []) ")" l
  | (Sign ";") :: (Sign ")")  :: l -> create_sign_token_rep poss_s_beg i ((Sign ";") :: (Sign ")") :: []) ")" l
  | (Sign ";") :: (Sign "-") :: (Sign ")") :: l -> create_sign_token_rep poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign ")") :: []) ")" l
  (* | (Sign ";") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (";-))))","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (":-))))","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (":-)))","sinterj"))
  | (Sign ";") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (";-)))","sinterj"))
  | (Sign ";") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (";)))","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (":-))","sinterj"))
  | (Sign ";") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (";-))","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign ")") :: []) l (make_lemma (":-)","sinterj"))
  | (Sign ";") :: (Sign "-") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign ")") :: []) l (make_lemma (";-)","sinterj"))
  | (Sign ":") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign ")") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (":)))","sinterj"))
  | (Sign ":") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (":))","sinterj"))
  | (Sign ";") :: (Sign ")") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign ")") :: (Sign ")") :: []) l (make_lemma (";))","sinterj")) *)
  | (Sign ":") :: (Sign "|") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "|") :: []) l (make_lemma (":|","sinterj"))
  | (Sign ":") :: (Sign "\\") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "\\") :: []) l (make_lemma (":\\","sinterj"))
  | (Sign ":") :: (Sign "/") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "/") :: []) l (make_lemma (":/","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Sign "/") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign "/") :: []) l (make_lemma (":-/","sinterj"))
  (* | (Sign ":") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign ")") :: []) l (make_lemma (":)","sinterj"))
  | (Sign ";") :: (Sign ")") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign ")") :: []) l (make_lemma (";)","sinterj")) *)
  | (Sign "[") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "]") :: l -> create_sign_token poss_s_beg i ((Sign "[") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "]") :: []) l (make_lemma ("(…)","sinterj"))
  | (Sign "[") :: (Sign ".") :: (Sign ".") :: (Sign "]") :: l -> create_sign_token poss_s_beg i ((Sign "[") :: (Sign ".") :: (Sign ".") :: (Sign "]") :: []) l (make_lemma ("(…)","sinterj"))
  | (Sign "[") :: (Sign "+") :: (Sign "]") :: l -> create_sign_token poss_s_beg i ((Sign "[") :: (Sign "+") :: (Sign "]") :: []) l (make_lemma ("[+]","symbol"))
  | (Sign "[") :: (Sign "-") :: (Sign "]") :: l -> create_sign_token poss_s_beg i ((Sign "[") :: (Sign "-") :: (Sign "]") :: []) l (make_lemma ("[-]","symbol"))
  | (Sign "[") :: (Sign "?") :: (Sign "]") :: l -> create_sign_token poss_s_beg i ((Sign "[") :: (Sign "?") :: (Sign "]") :: []) l (make_lemma ("[?]","sinterj"))
  | (Sign ":") :: (Sign "]") :: l ->
      let t,i2 = create_empty_sign_token i [Sign ":";Sign "]"] in
      Variant[Token{t with token=make_lemma (":]","sinterj")};
              Seq[Token{empty_token_env with orth=":";beg=i;len=factor;next=i+factor;token=Interp ":"; attrs=[MaybeCS]};
                  Token{empty_token_env with orth="]";beg=i+factor;len=factor;next=i+2*factor;token=Interp "]"; attrs=[MaybeCS]}]],i2,l,false
  | (Sign ";") :: (Sign "]") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "]") :: []) l (make_lemma (";]","sinterj"))
  | (Sign ":") :: (Capital("P",_)) :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "P") :: []) l (make_lemma (":P","sinterj"))
  | (Sign ";") :: (Capital("P",_)) :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "P") :: []) l (make_lemma (";P","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Capital("P",_)) :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign "P") :: []) l (make_lemma (":-P","sinterj"))
  | (Sign ";") :: (Sign "-") :: (Capital("P",_)) :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign "P") :: []) l (make_lemma (";-P","sinterj"))
  | (Sign ":") :: (Capital("D",_)) :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "D") :: []) l (make_lemma (":D","sinterj"))
  | (Sign ";") :: (Capital("D",_)) :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "D") :: []) l (make_lemma (";D","sinterj"))
  | (Sign ":") :: (Sign "-") :: (Capital("D",_)) :: l -> create_sign_token poss_s_beg i ((Sign ":") :: (Sign "-") :: (Sign "D") :: []) l (make_lemma (":-D","sinterj"))
  | (Sign ";") :: (Sign "-") :: (Capital("D",_)) :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "-") :: (Sign "D") :: []) l (make_lemma (";-D","sinterj"))
  | (Sign "]") :: l -> create_sign_token poss_s_beg i [Sign "]"] l (Interp "]")
  | (Sign "[") :: l -> create_sign_token poss_s_beg i [Sign "["] l (Interp "[")
  | (Sign "'") :: (Sign "'") :: (Sign ".") :: l -> create_quot_digit_token i [Sign "'";Sign "'"] l
  | (Sign "'") :: (Sign "'") :: l ->
      let t,i = create_empty_sign_token i [Sign "'";Sign "'"] in
      Variant[Token{t with token=Interp "”"};Token{t with token=Interp "”s"}],i,l,poss_s_beg
  | (Sign "’") :: (Sign "’") :: (Sign ".") :: l -> create_quot_digit_token i [Sign "’";Sign "’"] l
  | (Sign "’") :: (Sign "’") :: l ->
      let t,i = create_empty_sign_token i [Sign "’";Sign "’"] in
      Variant[Token{t with token=Interp "”"};Token{t with token=Interp "”s"}],i,l,poss_s_beg
  | (Sign ";") :: (Sign "*") :: l -> create_sign_token poss_s_beg i ((Sign ";") :: (Sign "*") :: []) l (make_lemma (";*","sinterj"))
  | (Sign "?") :: (Sign "!") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "!") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "?!...",i+5*factor,l,true
  | (Sign "?") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "?...",i+4*factor,l,true
  | (Sign "?") :: (Sign "?") :: (Sign "?") :: (Sign "?") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "?") :: (Sign "?") :: (Sign "?") :: []) l "????",i+4*factor,l,true
  | (Sign "?") :: (Sign "!") :: (Sign "!") :: (Sign "!") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "!") :: (Sign "!") :: (Sign "!") :: []) l "?!!!",i+4*factor,l,true
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "?") :: l ->
        Variant[create_sentence_seq_hapl_q i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "?") :: []) l "…?";
                create_sentence_seq_q i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "?") :: []) l "…?"],i+4*factor,l,true
  | (Sign "?") :: (Sign "!") :: (Sign "?") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "!") :: (Sign "?") :: []) l "?!?",i+3*factor,l,true
  | (Sign "?") :: (Sign "?") :: (Sign "?") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "?") :: (Sign "?") :: []) l "???",i+3*factor,l,true
  | (Sign "?") :: (Sign "!") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "!") :: []) l "?!",i+2*factor,l,true
  | (Sign "?") :: (Sign "?") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "?") :: []) l "??",i+2*factor,l,true
(*   | (Sign "?") :: (Sign ".") :: l -> *)
  | (Sign "!") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        create_sentence_seq_q i ((Sign "!") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "!...",i+4*factor,l,true
  | (Sign "!") :: (Sign "?") :: l ->
        create_sentence_seq_q i ((Sign "!") :: (Sign "?") :: []) l "!?",i+2*factor,l,true
  | (Sign "?") :: (Sign "…") :: l ->
        create_sentence_seq_q i ((Sign "?") :: (Sign "…") :: []) l "?…",i+2*factor,l,true
  | (Sign "…") :: (Sign "?") :: l ->
        Variant[create_sentence_seq_hapl_q i ((Sign "…") :: (Sign "?") :: []) l "…?";
                create_sentence_seq_q i ((Sign "…") :: (Sign "?") :: []) l "…?"],i+2*factor,l,true
  | (Sign "?") :: l ->
        create_sentence_seq_q i ((Sign "?") :: []) l "?",i+factor,l,true
  | (Sign "!") :: (Sign "!") :: (Sign "!") :: (Sign "!") :: l ->
        create_sentence_seq i ((Sign "!") :: (Sign "!") :: (Sign "!") :: (Sign "!") :: []) l "!!!!",i+4*factor,l,true
  | (Sign "!") :: (Sign "!") :: (Sign "!") :: l ->
        create_sentence_seq i ((Sign "!") :: (Sign "!") :: (Sign "!") :: []) l "!!!",i+3*factor,l,true
  | (Sign "!") :: (Sign "!") :: l ->
        create_sentence_seq i ((Sign "!") :: (Sign "!") :: []) l "!!",i+2*factor,l,true
  | (Sign "!") :: l ->
        create_sentence_seq i ((Sign "!") :: []) l "!",i+factor,l,true
  | (Sign "…") :: l ->
      if is_multidot_sentence_end_marker l then
        Variant[create_sentence_seq_hapl i ((Sign "…") :: []) l "…";
                create_sentence_seq i ((Sign "…") :: []) l "…"],i+factor,l,true
      else
        Variant[create_sentence_seq_hapl i ((Sign "…") :: []) l "…";
                create_sentence_seq i ((Sign "…") :: []) l "…";
                Token{empty_token_env with orth="…";beg=i;len=factor;next=i+factor;token=make_lemma ("…","sinterj"); attrs=[MaybeCS]}],i+factor,l,true
  | (Sign "/") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "/") :: l -> create_sign_token poss_s_beg i ((Sign "/") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign "/") :: []) l (make_lemma ("(…)","sinterj"))
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l -> (* Różne natęrzenia wielokropka i wypunktowania *)
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……"],i+8*factor,l,true
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……"],i+7*factor,l,true
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……"],i+6*factor,l,true
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……"],i+5*factor,l,true
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "……"],i+4*factor,l,true
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: l ->
      if is_multidot_sentence_end_marker l then
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "…";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "…"],i+3*factor,l,true
      else
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "…";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: (Sign ".") :: []) l "…";
                Token{empty_token_env with orth="...";beg=i;len=3*factor;next=i+3*factor;token=make_lemma ("…","sinterj"); attrs=[MaybeCS]}],i+3*factor,l,true
  | (Sign ".") :: (Sign ".") :: l ->
      if is_multidot_sentence_end_marker l then
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: []) l "…";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: []) l "…"],i+2*factor,l,true
      else
        Variant[create_sentence_seq_hapl i ((Sign ".") :: (Sign ".") :: []) l "…";
                create_sentence_seq i ((Sign ".") :: (Sign ".") :: []) l "…";
                Token{empty_token_env with orth="..";beg=i;len=2*factor;next=i+2*factor;token=make_lemma ("…","sinterj"); attrs=[MaybeCS]}],i+2*factor,l,true
  | (Sign "*") :: (Sign "*") :: (Sign "*") :: (Sign "*") :: (Sign "*") :: l -> create_sign_token poss_s_beg i [Sign "*";Sign "*";Sign "*";Sign "*";Sign "*"] l (Interp "*****") (* zastępniki liter *)
  | (Sign "*") :: (Sign "*") :: (Sign "*") :: (Sign "*") :: l -> create_sign_token poss_s_beg i [Sign "*";Sign "*";Sign "*";Sign "*"] l (Interp "****")
  | (Sign "*") :: (Sign "*") :: (Sign "*") :: l -> create_sign_token poss_s_beg i [Sign "*";Sign "*";Sign "*"] l (Interp "***")
  | (Sign "*") :: (Sign "*") :: l -> create_sign_token poss_s_beg i [Sign "*";Sign "*"] l (Interp "**")
  | (Sign "*") :: l -> (* Interp to zastępnik liter i cudzysłów, symbol listy *)
      let t,i2 = create_empty_sign_token i [Sign "*"] in
      Variant([Token{t with token=Interp "*"};Token{t with token=Symbol "*"}] @
      (if !internet_mode then [sc_dig_token "*" i [Sign "*"] (make_lemma ("*","symbol"))] else [])),i2,l,poss_s_beg
  | (Sign "+") :: l -> (* Interp to spójnik *)
      let t,i2 = create_empty_sign_token i [Sign "+"] in
      Variant[Token{t with token=Interp "+"};Token{t with token=Symbol "+"}],i2,l,poss_s_beg
  | (Sign "«") :: l ->
      let t,i = create_empty_sign_token i [Sign "«"] in
      Variant[Token{t with token=Interp "«"};Token{t with token=Interp "«s"}],i,l,poss_s_beg
  | (Sign "»") :: l ->
      let t,i = create_empty_sign_token i [Sign "»"] in
      Variant[Token{t with token=Interp "»"};Token{t with token=Interp "»s"}],i,l,poss_s_beg
  | (Sign "<") :: (Sign "<") :: l -> create_sign_token poss_s_beg i [Sign "<";Sign "<"] l (Interp "«") (* prawy cudzysłów *)
  | (Sign "<") :: (Digit "3") :: l -> create_sign_token poss_s_beg i [Sign "<";Sign "3"] l (make_lemma ("<3","sinterj"))
  | (Sign "<") :: l -> (* prawy cudzysłów i element wzoru matematycznego *)
      let t,i = create_empty_sign_token i [Sign "<"] in
      Variant[Token{t with token=Interp "«"};Token{t with token=Symbol "<"}],i,l,poss_s_beg
  | (Sign ">") :: (Sign ">") :: l -> create_sign_token poss_s_beg i [Sign ">";Sign ">"] l (Interp "»") (* lewy cudzysłów *)
  | (Sign ">") :: l -> create_sign_token poss_s_beg i [Sign ">"] l (Symbol ">")
  | (Sign "‘") :: l -> create_sign_token poss_s_beg i [Sign "‘"] l (Interp "‘")
  | (Sign "`") :: (Sign "`") :: l ->
      let t,i2 = create_empty_sign_token i [Sign "`";Sign "`"] in
      Variant[sc_dig_token "``" i [Sign "`";Sign "`"] (Interp "„x"); Token{t with token=Interp "„"};Token{t with token=Interp "„s"}],i2,l,poss_s_beg
  | (Sign "·") :: l -> create_sign_token poss_s_beg i [Sign "·"] l (Interp "·")
  | (Sign "•") :: l -> create_sign_token poss_s_beg i [Sign "•"] l (Interp "•")
  | (Sign "¨") :: l -> create_sign_token poss_s_beg i [Sign "¨"] l (Interp "¨")
  | (Sign "~") :: l ->
      let t,i = create_empty_sign_token i [Sign "~"] in
      Variant[Token{t with token=Symbol "~"};Token{t with token=make_lemma ("około","prep:gen")}],i,l,false
  | (Sign "{") :: l ->
      let t,i = create_empty_sign_token i [Sign "{"] in
      Variant[Token{t with token=Symbol "{"};Token{t with token=Interp "{"}],i,l,poss_s_beg
  | (Sign "}") :: l ->
      let t,i = create_empty_sign_token i [Sign "}"] in
      Variant[Token{t with token=Symbol "}"};Token{t with token=Interp "}"}],i,l,poss_s_beg
  | (Sign "#") :: l -> create_sign_token poss_s_beg i [Sign "#"] l (Symbol "#")
  | (Sign "^") :: (Sign "^") :: l -> create_sign_token poss_s_beg i [Sign "^";Sign "^"] l (make_lemma ("^^","sinterj"))
  | (Sign "^") :: l -> create_sign_token poss_s_beg i [Sign "^"] l (Symbol "^")
  | (Sign "|") :: l -> create_sign_token poss_s_beg i [Sign "|"] l (Symbol "|")
  | (Sign "&") :: l ->
      let t,i = create_empty_sign_token i [Sign "&"] in
      Variant[Token{t with token=Symbol "&"};Token{t with token=make_lemma ("&","conj")}],i,l,false
  | (Sign "=") :: l -> create_sign_token poss_s_beg i [Sign "="] l (Symbol "=")
  | (Sign "/") :: l ->
      let t,i = create_empty_sign_token i [Sign "/"] in
      Variant[Token{t with token=Symbol "/"};Token{t with token=make_lemma ("na","prep:acc")}],i,l,false
  | (Sign "_") :: l -> create_sign_token poss_s_beg i [Sign "_"] l (Symbol "_")
  | (Sign "@") :: l -> create_sign_token poss_s_beg i [Sign "@"] l (Symbol "@")
  | (Sign "×") :: l -> create_sign_token poss_s_beg i [Sign "×"] l (Symbol "×")
  | (Sign "±") :: l -> create_sign_token poss_s_beg i [Sign "±"] l (make_lemma ("±","symbol"))
  | (Sign "%") :: l ->
      let t,i = create_empty_sign_token i [Sign "%"] in
      Variant[Token{t with token=Symbol "%"};Token{t with token=make_lemma ("procent","subst:_:_:m3")}],i,l,false
  | (Sign "$") :: l ->
      let t,i = create_empty_sign_token i [Sign "$"] in
      Variant[Token{t with token=Symbol "$"};Token{t with token=make_lemma ("dolar","subst:_:_:m2")}],i,l,false
  | (Sign "€") :: l -> create_sign_token poss_s_beg i [Sign "€"] l (make_lemma ("euro","subst:_:_:n:ncol"))
  | (Sign "²") :: l -> create_sign_token poss_s_beg i [Sign "²"] l (Symbol "²")
  | (Sign "°") :: l -> create_sign_token poss_s_beg i [Sign "°"] l (make_lemma ("stopień","subst:_:_:m3"))
  | (Sign "§") :: l -> create_sign_token false i [Sign "§"] l (make_lemma ("paragraf","subst:_:_:m3"))
  | (Sign "®") :: l -> create_sign_token poss_s_beg i [Sign "®"] l (make_lemma ("®","symbol"))
  | (Sign "™") :: l -> create_sign_token poss_s_beg i [Sign "™"] l (make_lemma ("™","symbol"))
  | (Sign "µ") :: l -> create_sign_token poss_s_beg i [Sign "µ"] l (Symbol "µ")
  | (Sign "μ") :: l -> create_sign_token poss_s_beg i [Sign "μ"] l (Symbol "µ")
  | (Sign "†") :: l -> create_sign_token poss_s_beg i [Sign "†"] l (Interp "†")
  | (Sign s) :: l -> print_endline ("recognize_sign_group: " ^ s); create_sign_token poss_s_beg i [Sign s] l (Symbol s)
  | l ->  failwith "recognize_sign_group"*)

let create_multidot i signs l =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_multidot")) in
  let len = Xlist.size signs * factor in
  Variant[Seq[Token{empty_token_env with beg=i;len=10;next=i+10;token=Symbol "."; attrs=[]};
              Token{empty_token_env with orth=orth;beg=i+10;len=factor-10;next=i+factor;token=Interp "…"}];
          Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "…"; attrs=[]}],i+len,l

let create_quot i signs l =
  let orth = String.concat "" (Xlist.map signs (function Sign s -> s | _ -> failwith "create_quot")) in
  let len = Xlist.size signs * factor in
  Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "’"},i+len,l
(*  Variant[Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Symbol "’"; attrs=[]};
          Token{empty_token_env with orth=orth;beg=i;len=len;next=i+len;token=Interp "’"; attrs=[]}],i+len,l*)

let recognize_sign_group i = function
  | (Sign "&") :: (Small(u1,"n")) :: (Small(u2,"b")) :: (Small(u3,"s")) :: (Small(u4,"p")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"n")) :: (Small(u2,"b")) :: (Small(u3,"s")) :: (Small(u4,"p")) :: (Sign ";") :: []) l (Symbol " ")
  | (Sign "&") :: (Small(u1,"a")) :: (Small(u2,"l")) :: (Small(u3,"p")) :: (Small(u4,"h")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"a")) :: (Small(u2,"l")) :: (Small(u3,"p")) :: (Small(u4,"h")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "α")
  | (Sign "&") :: (Small(u1,"b")) :: (Small(u2,"e")) :: (Small(u3,"t")) :: (Small(u4,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"b")) :: (Small(u2,"e")) :: (Small(u3,"t")) :: (Small(u4,"a")) :: (Sign ";") :: []) l (Interp "β")
  | (Sign "&") :: (Small(u1,"g")) :: (Small(u2,"a")) :: (Small(u3,"m")) :: (Small(u4,"m")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"g")) :: (Small(u2,"a")) :: (Small(u3,"m")) :: (Small(u4,"m")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "γ")
  | (Sign "&") :: (Small(u1,"k")) :: (Small(u2,"a")) :: (Small(u3,"p")) :: (Small(u4,"p")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"k")) :: (Small(u2,"a")) :: (Small(u3,"p")) :: (Small(u4,"p")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "κ")
   | (Sign "&") :: (Small(u1,"D")) :: (Small(u2,"e")) :: (Small(u3,"l")) :: (Small(u4,"t")) :: (Small(u5,"a")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"D")) :: (Small(u2,"e")) :: (Small(u3,"l")) :: (Small(u4,"t")) :: (Small(u5,"a")) :: (Sign ";") :: []) l (Interp "Δ")
  | (Sign "&") :: (Small(u1,"m")) :: (Small(u2,"u")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"m")) :: (Small(u2,"u")) :: (Sign ";") :: []) l (Interp "µ")
  | (Sign "&") :: (Small(u1,"g")) :: (Small(u2,"e")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"g")) :: (Small(u2,"e")) :: (Sign ";") :: []) l (Interp "≥")
  | (Sign "&") :: (Small(u1,"l")) :: (Small(u2,"e")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"l")) :: (Small(u2,"e")) :: (Sign ";") :: []) l (Interp "≤")
  | (Sign "&") :: (Small(u1,"g")) :: (Small(u2,"t")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"l")) :: (Small(u2,"e")) :: (Sign ";") :: []) l (Interp ">")
  | (Sign "&") :: (Small(u1,"l")) :: (Small(u2,"t")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"l")) :: (Small(u2,"e")) :: (Sign ";") :: []) l (Interp "<")
  | (Sign "&") :: (Small(u1,"u")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"u")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: []) l (Interp "↑")
  | (Sign "&") :: (Small(u1,"d")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"d")) :: (Small(u2,"a")) :: (Small(u3,"r")) :: (Small(u4,"r")) :: (Sign ";") :: []) l (Interp "↓")
  | (Sign "&") :: (Small(u1,"d")) :: (Small(u2,"e")) :: (Small(u3,"g")) :: (Sign ";") :: l -> create_sign_token i ((Sign "&") :: (Small(u1,"d")) :: (Small(u2,"e")) :: (Small(u3,"g")) :: (Sign ";") :: []) l (Interp "°")
  | (Sign " ") :: l -> create_sign_token i [Sign " "] l (Symbol " ")
  | (Sign "﻿") :: l -> create_sign_token i [Sign "﻿"] l (Symbol " ")
  | (Sign " ") :: l -> create_sign_token i [Sign " "] l (Symbol " ")
  | (Sign "\t") :: l -> create_sign_token i [Sign "\t"] l (Symbol "\t")
  | (Sign "\r") :: l -> create_sign_token i [Sign "\r"] l (Symbol "\r")
  | (Sign "\n") :: l -> create_sign_token i [Sign "\n"] l (Symbol "\n")
  | (Sign "'") :: l -> create_quot i [Sign "'"] l
  | (Sign "’") :: l -> create_quot i [Sign "’"] l
  | (Sign "´") :: l -> create_quot i [Sign "´"] l
  | (Sign "`") :: l -> create_quot i [Sign "`"] l
  | (Sign "-") :: (Sign "-") :: (Sign "-") :: l -> create_or_beg2 i [Sign "-";Sign "-";Sign "-"] l
  | (Sign "-") :: (Sign "-") :: l -> create_or_beg2 i [Sign "-";Sign "-"] l
  | (Sign "-") :: l -> create_or_beg i [Sign "-"] l
  | (Sign "‐") :: l -> create_or_beg i [Sign "‐"] l
  | (Sign "‑") :: l -> create_or_beg i [Sign "‑"] l
  | (Sign "‒") :: l -> create_or_beg i [Sign "‒"] l
  | (Sign "−") :: l -> create_or_beg i [Sign "−"] l
  | (Sign "–") :: l -> create_or_beg i [Sign "–"] l
  | (Sign "—") :: l -> create_or_beg i [Sign "—"] l
  | (Sign "…") :: l -> create_multidot i ((Sign "…") :: []) l
  | (Sign ".") :: (Sign ".") :: (Sign ".") :: l -> create_multidot i ((Sign ".") :: (Sign ".") :: (Sign ".") :: []) l
  | (Sign ".") :: (Sign ".") :: l -> create_multidot i ((Sign ".") :: (Sign ".") :: []) l
  | (Sign ".") :: l ->
        Variant[Seq[Token{empty_token_env with beg=i;len=10;next=i+10;token=Symbol "."; attrs=[]};
                    Token{empty_token_env with orth=".";beg=i+10;len=factor-10;next=i+factor;token=Interp "."}];
                Token{empty_token_env with orth=".";beg=i;len=factor;next=i+factor;token=Symbol "."; attrs=[]};
                Token{empty_token_env with orth=".";beg=i;len=factor;next=i+factor;token=Interp "."; attrs=[]}],i+factor,l
  | (Sign s) :: l -> create_sign_token i [Sign s] l (Interp s)
  | l ->  failwith "recognize_sign_group"

(* FIXME: "„Szpak” frunie." trzeba przenie przenieść <sentence> przed „, ale zostawić po „s. *)

(*let rec group_url rev = function
    Small s :: l -> group_url (s :: rev) l
  | Capital(s,t) :: l -> group_url (s :: rev) l
  | ForeignSmall s :: l -> group_url (s :: rev) l
  | ForeignCapital(s,t) :: l -> group_url (s :: rev) l
  | Digit s :: l -> group_url (s :: rev) l
  | Sign "." :: l -> group_url ("." :: rev) l
  | Sign "-" :: l -> group_url ("-" :: rev) l
  | Sign "/" :: l -> group_url ("/" :: rev) l
  | Sign ":" :: l -> group_url (":" :: rev) l
  | Sign "%" :: l -> group_url ("%" :: rev) l
  | Sign "#" :: l -> group_url ("#" :: rev) l
  | Sign "?" :: l -> group_url ("?" :: rev) l
  | Sign "=" :: l -> group_url ("=" :: rev) l
  | Sign "," :: l -> group_url ("," :: rev) l
  | Sign "~" :: l -> group_url ("~" :: rev) l
  | Sign "_" :: l -> group_url ("_" :: rev) l
  | l -> List.rev rev, l*)

let merge_url i len orth cat =
    Token{empty_token_env with orth=orth;beg=i;len=len*factor;next=i+len*factor;token=Dig(orth,cat)}

let rec group_chars i rev = function
    [] -> List.rev ((Token{empty_token_env with beg=i;len=factor;next=i+factor;token=Interp "</query>"}) :: rev)
  (* | (Small "h") :: (Small "t") :: (Small "t") :: (Small "p") :: (Sign ":") :: (Sign "/") :: (Sign "/") :: _ as l ->
       let x,l = group_url [] l in group_chars false (i + Xlist.size x * factor) ((merge_url i x) :: rev) l
  | (Small "h") :: (Small "t") :: (Small "t") :: (Small "p") :: (Small "s") :: (Sign ":") :: (Sign "/") :: (Sign "/") :: _ as l ->
       let x,l = group_url [] l in group_chars false (i + Xlist.size x * factor) ((merge_url i x) :: rev) l *)
  | Digit s :: l -> let x,l = group_digits [] ((Digit s) :: l) in group_chars (i + Xlist.size x * factor) ((merge_digits i x) :: rev) l
  | Sign s :: l -> let x,i,l = recognize_sign_group i ((Sign s) :: l) in group_chars i (x :: rev) l
  | Capital(s,t) :: l -> let x,l = group_letters [] ((Capital(s,t)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | ForeignCapital(s,t) :: l -> let x,l = group_letters [] ((ForeignCapital(s,t)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | Small(uc,lc) :: l -> let x,l = group_letters [] ((Small(uc,lc)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | ForeignSmall(uc,lc) :: l -> let x,l = group_letters [] ((ForeignSmall(uc,lc)) :: l) in group_chars (i + Xlist.size x * factor) ((merge_letters i x) :: rev) l
  | Emoticon s :: l -> group_chars (i + factor) ((Token{empty_token_env with orth=s;beg=i;len=factor;next=i+factor;token=make_lemma (s,"sinterj")}) :: rev) l
  | Other("url",len) :: Sign s :: l -> group_chars (i + len * factor) ((merge_url i len s "url") :: rev) l
  | Other("email",len) :: Sign s :: l -> group_chars (i + len * factor) ((merge_url i len s "email") :: rev) l
  | Other(s,x) :: l ->
        let x,l = group_others [] ((Other(s,x)) :: l) in
        group_chars (i + Xlist.size x * factor)
          ((Token{empty_token_env with orth=String.concat "" x;beg=i;len=Xlist.size x * factor;next=i+Xlist.size x * factor;token=Other(String.concat "" x)}) :: rev) l

let tokenize l =
  (Token{empty_token_env with beg=0;len=factor;next=factor;token=Interp "<query>"}) :: (group_chars factor [] (ENIAMurl.find l))
