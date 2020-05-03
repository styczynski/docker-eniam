(*
 *  XT, a library that converts XLE output into ENIAM format.
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

open XTTypes
open Xstd

let rec get_attribute e = function
    [] -> LVar ""
  | (e2,v) :: l -> if e = e2 then v else get_attribute e l

let pred_of_ptype2 l =
  match get_attribute "PFORM" l with
    LVar "" -> l
  | Cons(_,s) -> ["PRED",QCons(StringMap.empty,s,"",[Cons(StringMap.empty,"OBJ")],[],-1,-1);
               "OBJ",Compound(StringMap.empty,l);
               "CHECK",Compound(StringMap.empty,["_CAT",Cons(StringMap.empty,"prep")]);
               "PTYPE",get_attribute "PTYPE" l;
               "PCASE",get_attribute "PCASE" l]
  | _ -> failwith "pred_of_ptype2"

let rec pred_of_ptype = function
    Cons _ as t -> t
  | QCons _ as t -> t
  | LVar _ as t -> t
  | Compound(ids,l) ->
      let l = Xlist.map l (fun (e,t) -> e, pred_of_ptype t) in
      Compound(ids,pred_of_ptype2 l)
  | Set(ids,l) -> Set(ids,Xlist.map l pred_of_ptype)
  | Coordination(ids,l,l2) ->
      Coordination(ids,Xlist.map l pred_of_ptype, Xlist.map l2 (fun (e,t) -> e, pred_of_ptype t))
  | Loop _ as t -> t
  | Context l -> Context(Xlist.map l (fun (c,t) -> c,pred_of_ptype t))

let rec get_alternatives found (ht : string) hi hcat he hc = function
    Cons _ -> found
  | QCons _ -> found
  | LVar _ -> found
  | Compound(_,l) ->
       let t,i =
         match get_attribute "PRED" l with
             QCons(_,t,i,_,_,_,_) -> t, i
           | LVar "" -> "",""
           | _ -> failwith "get_alternatives" in
       let cat =
         match get_attribute "CHECK" l with
             Compound(_,l) -> (match get_attribute "_CAT" l with Cons(_,cat) -> cat | LVar "" -> "" | _ -> failwith "get_alternatives")
           | LVar "" -> ""
           | _ -> failwith "get_alternatives" in
       let found = if t = "" then found else (ht,hi,hcat,he,hc,t,i,cat) :: found in
       Xlist.fold l found (fun found (e,tree) -> get_alternatives found t i cat e hc tree)
  | Set(_,l) -> Xlist.fold l found (fun found t -> get_alternatives found ht hi hcat he hc t)
  | Coordination(_,l2,l) -> (* FIXME: to wymaga jeszcze do przemyślenia *)
       Xlist.fold l found (fun found (e,t) -> get_alternatives found ht hi hcat e hc t)
  | Loop _ -> found
  | Context l ->
       Xlist.fold l found (fun found (c,t) -> get_alternatives found ht hi hcat he (XTContext.intersection (hc,c)) t)

let simplify_cat = function
    "fin" -> "verb"
  | "praet" -> "verb"
  | "impt" -> "verb"
  | "imps" -> "verb"
  | "winien" -> "verb"
  | "pcon" -> "verb"
  | "pant" -> "verb"
  | "pred" -> "verb"
  | "subst" -> "noun"
  | "depr" -> "noun"
  | "ppron12" -> "noun"
  | "ppron3" -> "noun"
  | s -> s

let remove_unambiguous map =
  StringMap.fold map StringMap.empty (fun new_map t (cat,map) ->
    if StringMap.size map > 1 then StringMap.add new_map t (cat,map) else
    let map = StringMap.fold map StringMap.empty (fun new_map ht (hcat,map) ->
      if StringMap.size map > 1 then StringMap.add new_map ht (hcat,map) else new_map) in
    if StringMap.size map > 0 then StringMap.add new_map t (cat,map) else new_map)

let gfrank = Xlist.fold [
  "SUBJ", 5;
  "OBJ", 5;
  "OBJ-TH", 5;
  "OBL-AG", 5;
  "OBL-ABL", 5;
  "OBL-ADL", 5;
  "OBL-DUR", 5;
  "OBL-LOCAT", 5;
  "OBL-MOD", 5;
  "OBL-PERL", 5;
  "OBL-TEMP", 5;
  "COMP", 5;
  "XCOMP", 5;
  "XCOMP-PRED", 5;
  "OBL-ADV", 4;
  "OBL", 4;
  "OBL2", 4;
  "OBL3", 4;
  "OBL4", 4;
  "OBL-INST", 4;
  "OBL-STR", 4;
  "OBL-GEN", 4;
  "POSS", 3;
  "APP", 3;
  "ADJUNCT", 2;
  "XADJUNCT", 2;
  "GFWEIRD", 1] StringMap.empty (fun map (k,v) -> StringMap.add map k v)

let gfrank_kasia = Xlist.fold [
  "ADJUNCT", 3.680;
  "APP", 2.591;
  "COMP", 7.348;
  "OBJ", 5.963;
  "OBJ-TH", 6.286;
  "OBL", 5.380;
  "OBL-ABL", 6.212;
  "OBL-ADL", 5.407;
  "OBL-ADV", 5.082;
  "OBL-AG", 8.846;
  "OBL-COMPAR", 1.429;
  "OBL-DUR", 4.062;
  "OBL-GEN", 4.178;
  "OBL-INST", 5.217;
  "OBL-LOCAT", 4.641;
  "OBL-MOD", 4.044;
  "OBL-PERL", 3.061;
  "OBL-STR", 3.873;
  "OBL-TEMP", 1.345;
  "OBL2", 3.000;
  "POSS", 2.860;
  "SUBJ", 4.901;
  "XADJUNCT", 0.668;
  "XCOMP", 8.845;
  "XCOMP-PRED", 5.055;
  "GFWEIRD", 0.01] StringMap.empty (fun map (k,v) -> StringMap.add map k v)

let gfrank2 = Xlist.fold [
  "verb","ADJUNCT","noun", 1;
  "num","OBJ","noun", 10;
(*   "num","APP","noun", 5; *)
  "ppas","OBL-AG","prep", 10;
  "ppas","ADJUNCT","prep", 2;
  "noun","ADJUNCT","prep", 2;
  "adv","ADJUNCT","prep", 1;
  "adj","ADJUNCT","prep", 1;
  "verb","ADJUNCT","prep", 2;
  "verb","OBL-ABL","prep", 3;
  "verb","OBL-ADL","prep", 3;
  "verb","OBL-DUR","prep", 3;
  "verb","OBL-LOCAT","prep", 3;
  "verb","OBL-PERL","prep", 3;
  "verb","OBL-TEMP","prep", 3;
  "verb","OBL-MOD","prep", 3;
  "num","ADJUNCT","prep", 1;
  "verb","ADJUNCT","adv", 5;
  "prep","ADJUNCT","adv", 2;
  "verb","OBJ","adj", 1;
  "verb","XADJUNCT","adj", 1;
  "noun","ADJUNCT","adj", 5;
  ] StringMap.empty (fun map (k,l,m,v) -> StringMap.add map (k^"_"^l^"_"^m) v)

let gfrank2_kasia = Xlist.fold [
"adj", "ADJUNCT",  "adj",0.606;
"adj", "ADJUNCT",  "adv",6.721;
"adj", "ADJUNCT",  "noun",0.000;
"adj", "ADJUNCT",  "num",8.333;
"adj", "ADJUNCT",  "pact",0.000;
"adj", "ADJUNCT",  "ppas",0.000;
"adj", "ADJUNCT",  "prep",1.627;
"adj", "ADJUNCT",  "qub",3.043;
"adj", "ADJUNCT",  "siebie",0.000;
"adj", "ADJUNCT",  "verb",0.000;
"adj", "COMP",  "inf",10.000;
"adj", "OBJ",  "adj",0.000;
"adj", "OBJ",  "inf",0.000;
"adj", "OBJ",  "noun",10.000;
"adj", "POSS",  "adj",0.000;
"adj", "POSS",  "ger",0.000;
"adj", "POSS",  "noun",0.032;
"adj", "POSS",  "num",0.000;
"adj", "POSS",  "pact",0.000;
"adj", "POSS",  "prep",0.000;
"adj", "SUBJ",  "adj",0.111;
"adj", "SUBJ",  "comp",2.222;
"adj", "SUBJ",  "ger",2.500;
"adj", "SUBJ",  "inf",0.000;
"adj", "SUBJ",  "noun",1.664;
"adj", "SUBJ",  "num",1.250;
"adj", "SUBJ",  "pact",0.000;
"adj", "SUBJ",  "ppas",0.000;
"adj", "SUBJ",  "prep",0.000;
"adj", "SUBJ",  "verb",0.000;
"adj", "XCOMP",  "inf",10.000;
"adv", "ADJUNCT",  "adv",5.027;
"adv", "ADJUNCT",  "prep",0.500;
"adv", "ADJUNCT",  "qub",5.205;
"adv", "COMP",  "verb",10.000;
"adv", "OBL-COMPAR",  "prep",1.429;
"comp", "COMP",  "inf",6.250;
"comp", "COMP",  "verb",6.871;
"ger", "ADJUNCT",  "adj",8.000;
"ger", "ADJUNCT",  "adv",0.000;
"ger", "ADJUNCT",  "noun",0.000;
"ger", "ADJUNCT",  "pact",0.000;
"ger", "ADJUNCT",  "prep",0.702;
"ger", "ADJUNCT",  "qub",5.000;
"ger", "ADJUNCT",  "siebie",0.000;
"ger", "APP",  "noun",0.000;
"ger", "APP",  "num",0.000;
"ger", "COMP",  "verb",0.000;
"ger", "OBJ",  "adj",0.250;
"ger", "OBJ",  "ger",10.000;
"ger", "OBJ",  "noun",6.556;
"ger", "OBJ",  "num",6.667;
"ger", "OBJ",  "pact",0.000;
"ger", "OBJ",  "ppas",0.000;
"ger", "OBJ-TH",  "noun",5.000;
"ger", "OBJ-TH",  "siebie",10.000;
"ger", "OBL",  "adj",0.000;
"ger", "OBL",  "ger",10.000;
"ger", "OBL",  "noun",7.826;
"ger", "OBL",  "pact",0.000;
"ger", "OBL-ADL",  "prep",5.714;
"ger", "OBL-GEN",  "adj",0.000;
"ger", "OBL-GEN",  "noun",0.000;
"ger", "OBL-GEN",  "num",0.000;
"ger", "OBL-INST",  "noun",6.667;
"ger", "OBL-LOCAT",  "prep",2.500;
"ger", "OBL-MOD",  "adv",0.000;
"ger", "OBL-STR",  "adj",0.000;
"ger", "OBL-STR",  "noun",0.000;
"ger", "OBL-STR",  "num",0.000;
"ger", "POSS",  "adj",0.000;
"ger", "POSS",  "ger",0.000;
"ger", "POSS",  "noun",0.063;
"ger", "POSS",  "num",0.000;
"ger", "POSS",  "pact",0.000;
"ger", "POSS",  "ppas",0.000;
"ger", "SUBJ",  "adj",0.000;
"ger", "SUBJ",  "comp",0.000;
"ger", "SUBJ",  "ger",0.000;
"ger", "SUBJ",  "inf",0.000;
"ger", "SUBJ",  "noun",4.801;
"ger", "SUBJ",  "num",0.000;
"ger", "SUBJ",  "pact",0.000;
"ger", "SUBJ",  "ppas",0.000;
"ger", "SUBJ",  "verb",5.000;
"ger", "XCOMP-PRED",  "adj",3.333;
"ger", "XCOMP-PRED",  "noun",6.667;
"inf", "ADJUNCT",  "adv",5.600;
"inf", "ADJUNCT",  "comp",3.000;
"inf", "ADJUNCT",  "ger",0.000;
"inf", "ADJUNCT",  "noun",0.106;
"inf", "ADJUNCT",  "num",0.000;
"inf", "ADJUNCT",  "prep",2.546;
"inf", "ADJUNCT",  "qub",3.023;
"inf", "ADJUNCT",  "siebie",1.429;
"inf", "ADJUNCT",  "verb",0.000;
"inf", "COMP",  "inf",0.000;
"inf", "COMP",  "verb",5.333;
"inf", "OBJ",  "adj",0.625;
"inf", "OBJ",  "ger",10.000;
"inf", "OBJ",  "inf",0.000;
"inf", "OBJ",  "noun",8.056;
"inf", "OBJ",  "num",8.333;
"inf", "OBJ",  "pact",0.000;
"inf", "OBJ",  "ppas",0.000;
"inf", "OBJ",  "prep",3.333;
"inf", "OBJ",  "siebie",10.000;
"inf", "OBJ",  "verb",6.923;
"inf", "OBJ-TH",  "adj",1.667;
"inf", "OBJ-TH",  "noun",7.027;
"inf", "OBJ-TH",  "prep",0.000;
"inf", "OBJ-TH",  "siebie",10.000;
"inf", "OBL",  "adj",1.000;
"inf", "OBL",  "ger",10.000;
"inf", "OBL",  "noun",7.030;
"inf", "OBL",  "num",10.000;
"inf", "OBL",  "ppas",5.000;
"inf", "OBL",  "siebie",5.000;
"inf", "OBL-ABL",  "prep",4.286;
"inf", "OBL-ADL",  "prep",4.000;
"inf", "OBL-ADV",  "adv",2.222;
"inf", "OBL-DUR",  "noun",0.000;
"inf", "OBL-GEN",  "adj",0.000;
"inf", "OBL-GEN",  "ger",5.000;
"inf", "OBL-GEN",  "noun",5.000;
"inf", "OBL-GEN",  "ppas",0.000;
"inf", "OBL-INST",  "adj",0.000;
"inf", "OBL-INST",  "noun",6.667;
"inf", "OBL-INST",  "num",10.000;
"inf", "OBL-INST",  "prep",0.000;
"inf", "OBL-LOCAT",  "adv",10.000;
"inf", "OBL-LOCAT",  "prep",5.263;
"inf", "OBL-MOD",  "adv",4.000;
"inf", "OBL-MOD",  "prep",6.667;
"inf", "OBL-PERL",  "adj",0.000;
"inf", "OBL-PERL",  "noun",0.000;
"inf", "OBL-PERL",  "prep",10.000;
"inf", "OBL-STR",  "adj",0.000;
"inf", "OBL-STR",  "noun",2.500;
"inf", "OBL-STR",  "num",10.000;
"inf", "OBL-TEMP",  "adj",0.000;
"inf", "OBL-TEMP",  "adv",10.000;
"inf", "OBL-TEMP",  "noun",0.000;
"inf", "OBL-TEMP",  "prep",0.000;
"inf", "OBL2",  "adj",0.000;
"inf", "OBL2",  "noun",10.000;
"inf", "SUBJ",  "adj",8.000;
"inf", "SUBJ",  "comp",2.000;
"inf", "SUBJ",  "inf",0.000;
"inf", "SUBJ",  "noun",6.619;
"inf", "SUBJ",  "num",10.000;
"inf", "SUBJ",  "ppas",0.000;
"inf", "SUBJ",  "prep",0.000;
"inf", "SUBJ",  "verb",3.750;
"inf", "XADJUNCT",  "adj",0.154;
"inf", "XADJUNCT",  "pact",5.000;
"inf", "XADJUNCT",  "ppas",0.714;
"inf", "XCOMP",  "inf",7.500;
"inf", "XCOMP-PRED",  "adj",7.143;
"inf", "XCOMP-PRED",  "noun",6.471;
"inf", "XCOMP-PRED",  "ppas",8.750;
"noun", "ADJUNCT",  "adj",9.477;
"noun", "ADJUNCT",  "adv",0.423;
"noun", "ADJUNCT",  "inf",0.000;
"noun", "ADJUNCT",  "noun",0.100;
"noun", "ADJUNCT",  "num",0.000;
"noun", "ADJUNCT",  "pact",8.293;
"noun", "ADJUNCT",  "ppas",8.780;
"noun", "ADJUNCT",  "prep",2.262;
"noun", "ADJUNCT",  "qub",3.055;
"noun", "ADJUNCT",  "siebie",0.000;
"noun", "ADJUNCT",  "verb",6.875;
"noun", "APP",  "ger",0.000;
"noun", "APP",  "noun",3.272;
"noun", "APP",  "num",1.818;
"noun", "APP",  "prep",0.000;
"noun", "COMP",  "inf",7.500;
"noun", "COMP",  "verb",5.294;
"noun", "POSS",  "adj",0.247;
"noun", "POSS",  "ger",3.300;
"noun", "POSS",  "noun",4.935;
"noun", "POSS",  "num",1.935;
"noun", "POSS",  "pact",1.176;
"noun", "POSS",  "ppas",0.714;
"noun", "POSS",  "prep",0.000;
"noun", "SUBJ",  "adj",0.125;
"noun", "SUBJ",  "comp",0.000;
"noun", "SUBJ",  "ger",10.000;
"noun", "SUBJ",  "inf",0.000;
"noun", "SUBJ",  "noun",1.653;
"noun", "SUBJ",  "num",5.000;
"noun", "SUBJ",  "ppas",0.000;
"noun", "SUBJ",  "prep",0.000;
"noun", "XCOMP",  "inf",1.667;
"num", "ADJUNCT",  "adj",1.481;
"num", "ADJUNCT",  "adv",0.000;
"num", "ADJUNCT",  "noun",0.000;
"num", "ADJUNCT",  "pact",0.000;
"num", "ADJUNCT",  "ppas",0.000;
"num", "ADJUNCT",  "prep",1.404;
"num", "ADJUNCT",  "qub",7.632;
"num", "APP",  "ger",0.000;
"num", "APP",  "noun",0.000;
"num", "APP",  "num",0.000;
"num", "APP",  "prep",0.000;
"num", "OBJ",  "adj",1.034;
"num", "OBJ",  "comp",0.000;
"num", "OBJ",  "inf",0.000;
"num", "OBJ",  "noun",6.284;
"num", "OBJ",  "pact",5.000;
"num", "OBJ",  "ppas",1.667;
"num", "OBJ",  "verb",0.000;
"num", "POSS",  "adj",0.000;
"num", "POSS",  "ger",0.000;
"num", "POSS",  "noun",0.364;
"num", "POSS",  "ppas",0.000;
"num", "SUBJ",  "adj",0.000;
"num", "SUBJ",  "noun",0.000;
"pact", "ADJUNCT",  "adj",0.000;
"pact", "ADJUNCT",  "adv",6.364;
"pact", "ADJUNCT",  "noun",0.000;
"pact", "ADJUNCT",  "prep",0.909;
"pact", "COMP",  "verb",10.000;
"pact", "OBJ",  "ger",10.000;
"pact", "OBJ",  "noun",3.333;
"pact", "OBJ-TH",  "noun",0.000;
"pact", "OBL",  "ger",10.000;
"pact", "OBL",  "noun",4.286;
"pact", "OBL-ABL",  "prep",10.000;
"pact", "OBL-ADL",  "prep",8.000;
"pact", "OBL-DUR",  "adv",10.000;
"pact", "OBL-GEN",  "noun",6.667;
"pact", "OBL-INST",  "noun",0.000;
"pact", "OBL-LOCAT",  "adv",10.000;
"pact", "OBL-LOCAT",  "prep",10.000;
"pact", "OBL-MOD",  "adv",0.000;
"pact", "OBL-PERL",  "noun",0.000;
"pact", "OBL-PERL",  "prep",10.000;
"pact", "OBL-STR",  "noun",3.333;
"pact", "OBL-TEMP",  "noun",0.000;
"pact", "POSS",  "ger",0.000;
"pact", "POSS",  "noun",0.000;
"pact", "SUBJ",  "adj",0.000;
"pact", "SUBJ",  "inf",0.000;
"pact", "SUBJ",  "noun",0.978;
"pact", "SUBJ",  "num",0.000;
"pact", "SUBJ",  "verb",0.000;
"pact", "XCOMP",  "inf",10.000;
"ppas", "ADJUNCT",  "adj",0.000;
"ppas", "ADJUNCT",  "adv",6.944;
"ppas", "ADJUNCT",  "noun",0.000;
"ppas", "ADJUNCT",  "prep",1.980;
"ppas", "ADJUNCT",  "qub",2.857;
"ppas", "OBJ-TH",  "adj",0.000;
"ppas", "OBJ-TH",  "noun",7.500;
"ppas", "OBL",  "adj",0.000;
"ppas", "OBL",  "noun",7.500;
"ppas", "OBL",  "num",3.333;
"ppas", "OBL-ABL",  "prep",10.000;
"ppas", "OBL-ADL",  "prep",6.667;
"ppas", "OBL-ADV",  "adv",10.000;
"ppas", "OBL-AG",  "adj",0.000;
"ppas", "OBL-AG",  "noun",9.565;
"ppas", "OBL-AG",  "num",10.000;
"ppas", "OBL-INST",  "noun",2.857;
"ppas", "OBL-INST",  "prep",0.000;
"ppas", "OBL-LOCAT",  "prep",7.000;
"ppas", "OBL-MOD",  "adv",10.000;
"ppas", "POSS",  "ger",0.000;
"ppas", "POSS",  "noun",0.000;
"ppas", "POSS",  "num",0.000;
"ppas", "SUBJ",  "adj",0.476;
"ppas", "SUBJ",  "comp",0.000;
"ppas", "SUBJ",  "ger",10.000;
"ppas", "SUBJ",  "inf",1.429;
"ppas", "SUBJ",  "noun",1.980;
"ppas", "SUBJ",  "num",0.000;
"ppas", "SUBJ",  "prep",0.000;
"ppas", "SUBJ",  "verb",0.000;
"ppas", "XCOMP-PRED",  "noun",6.667;
"prep", "ADJUNCT",  "adv",1.163;
"prep", "ADJUNCT",  "noun",0.000;
"prep", "ADJUNCT",  "ppas",0.000;
"prep", "ADJUNCT",  "prep",0.000;
"prep", "ADJUNCT",  "qub",4.248;
"prep", "OBJ",  "adj",0.444;
"prep", "OBJ",  "adjp",10.000;
"prep", "OBJ",  "ger",3.667;
"prep", "OBJ",  "noun",6.569;
"prep", "OBJ",  "num",7.671;
"prep", "OBJ",  "pact",0.000;
"prep", "OBJ",  "ppas",0.769;
"prep", "OBJ",  "prep",0.000;
"prep", "OBJ",  "siebie",6.522;
"prep", "POSS",  "adj",0.000;
"prep", "POSS",  "noun",0.000;
"prep", "SUBJ",  "noun",0.000;
"siebie", "ADJUNCT",  "prep",0.000;
"siebie", "ADJUNCT",  "qub",0.000;
"siebie", "ADJUNCT",  "verb",0.000;
"siebie", "POSS",  "noun",0.000;
"verb", "ADJUNCT",  "adj",0.000;
"verb", "ADJUNCT",  "adv",6.045;
"verb", "ADJUNCT",  "comp",7.279;
"verb", "ADJUNCT",  "ger",0.000;
"verb", "ADJUNCT",  "inf",0.000;
"verb", "ADJUNCT",  "noun",0.527;
"verb", "ADJUNCT",  "num",0.752;
"verb", "ADJUNCT",  "prep",2.864;
"verb", "ADJUNCT",  "qub",5.486;
"verb", "ADJUNCT",  "siebie",0.909;
"verb", "ADJUNCT",  "verb",2.400;
"verb", "COMP",  "inf",10.000;
"verb", "COMP",  "verb",8.148;
"verb", "OBJ",  "adj",0.429;
"verb", "OBJ",  "comp",0.000;
"verb", "OBJ",  "ger",7.333;
"verb", "OBJ",  "inf",2.222;
"verb", "OBJ",  "noun",7.976;
"verb", "OBJ",  "num",5.667;
"verb", "OBJ",  "pact",0.000;
"verb", "OBJ",  "ppas",0.000;
"verb", "OBJ",  "prep",0.000;
"verb", "OBJ",  "siebie",10.000;
"verb", "OBJ",  "verb",6.866;
"verb", "OBJ-TH",  "adj",0.000;
"verb", "OBJ-TH",  "ger",10.000;
"verb", "OBJ-TH",  "noun",6.316;
"verb", "OBJ-TH",  "num",10.000;
"verb", "OBJ-TH",  "pact",3.333;
"verb", "OBJ-TH",  "prep",0.000;
"verb", "OBJ-TH",  "siebie",9.333;
"verb", "OBL",  "adj",0.698;
"verb", "OBL",  "ger",8.800;
"verb", "OBL",  "noun",5.488;
"verb", "OBL",  "num",5.263;
"verb", "OBL",  "pact",0.000;
"verb", "OBL",  "ppas",0.000;
"verb", "OBL",  "siebie",7.000;
"verb", "OBL-ABL",  "adv",10.000;
"verb", "OBL-ABL",  "prep",5.962;
"verb", "OBL-ADL",  "adv",4.444;
"verb", "OBL-ADL",  "prep",5.505;
"verb", "OBL-ADV",  "adv",5.490;
"verb", "OBL-DUR",  "adj",0.000;
"verb", "OBL-DUR",  "adv",10.000;
"verb", "OBL-DUR",  "ger",0.000;
"verb", "OBL-DUR",  "noun",1.538;
"verb", "OBL-DUR",  "num",0.000;
"verb", "OBL-DUR",  "prep",7.500;
"verb", "OBL-DUR",  "siebie",0.000;
"verb", "OBL-GEN",  "adj",0.000;
"verb", "OBL-GEN",  "ger",7.500;
"verb", "OBL-GEN",  "noun",4.375;
"verb", "OBL-GEN",  "num",5.000;
"verb", "OBL-GEN",  "prep",0.000;
"verb", "OBL-INST",  "adj",0.000;
"verb", "OBL-INST",  "noun",6.638;
"verb", "OBL-INST",  "ppas",0.000;
"verb", "OBL-INST",  "prep",0.000;
"verb", "OBL-LOCAT",  "adv",6.087;
"verb", "OBL-LOCAT",  "prep",4.354;
"verb", "OBL-MOD",  "adv",4.074;
"verb", "OBL-MOD",  "prep",3.103;
"verb", "OBL-PERL",  "adj",0.000;
"verb", "OBL-PERL",  "noun",0.000;
"verb", "OBL-PERL",  "prep",6.667;
"verb", "OBL-STR",  "adj",0.112;
"verb", "OBL-STR",  "ger",0.000;
"verb", "OBL-STR",  "inf",0.000;
"verb", "OBL-STR",  "noun",5.122;
"verb", "OBL-STR",  "num",5.667;
"verb", "OBL-STR",  "pact",0.000;
"verb", "OBL-STR",  "ppas",0.000;
"verb", "OBL-STR",  "prep",0.000;
"verb", "OBL-STR",  "siebie",0.000;
"verb", "OBL-TEMP",  "adj",0.000;
"verb", "OBL-TEMP",  "adv",2.727;
"verb", "OBL-TEMP",  "ger",0.000;
"verb", "OBL-TEMP",  "noun",0.120;
"verb", "OBL-TEMP",  "num",0.000;
"verb", "OBL-TEMP",  "prep",4.800;
"verb", "OBL-TEMP",  "siebie",0.000;
"verb", "OBL2",  "ger",0.000;
"verb", "OBL2",  "noun",1.667;
"verb", "SUBJ",  "adj",1.511;
"verb", "SUBJ",  "comp",2.000;
"verb", "SUBJ",  "ger",9.474;
"verb", "SUBJ",  "inf",1.515;
"verb", "SUBJ",  "noun",7.502;
"verb", "SUBJ",  "num",9.189;
"verb", "SUBJ",  "ppas",0.000;
"verb", "SUBJ",  "prep",0.000;
"verb", "SUBJ",  "verb",4.706;
"verb", "XADJUNCT",  "adj",0.354;
"verb", "XADJUNCT",  "pact",1.765;
"verb", "XADJUNCT",  "ppas",0.443;
"verb", "XADJUNCT",  "verb",9.048;
"verb", "XCOMP",  "inf",8.970;
"verb", "XCOMP-PRED",  "adj",6.731;
"verb", "XCOMP-PRED",  "noun",3.048;
"verb", "XCOMP-PRED",  "num",0.000;
"verb", "XCOMP-PRED",  "ppas",9.302;
"verb", "XCOMP-PRED",  "prep",0.000;
  ] StringMap.empty (fun map (k,l,m,v) -> StringMap.add map (k^"_"^l^"_"^m) v)

(* FIXME: to może nie działać przy powielaniu węzła - współdzielonej koordynacji / kontroli / podnoszeniu *)
let disambiguate_gf map =
  StringMap.fold map CEmpty (fun best_c t (cat,map) ->
    let chosen = StringMap.fold map [] (fun chosen ht (hcat,map) ->
      StringMap.fold map chosen (fun chosen e c ->
        let remain_flag = Xlist.fold chosen true (fun remain_flag (ht2,hcat2,e2,c2) ->
          if ht = ht2 then
            try (StringMap.find gfrank_kasia e) >= (StringMap.find gfrank_kasia e2) && remain_flag
            with Not_found -> failwith ("disambiguate_gf: " ^ e ^ " or " ^ e2 ^ " not in gfrank")
          else
            let r = try StringMap.find gfrank2_kasia (hcat^"_"^e^"_"^cat) with Not_found -> 10. in
            let r2 = try StringMap.find gfrank2_kasia (hcat2^"_"^e2^"_"^cat) with Not_found -> 10. in
            r >= r2 && remain_flag) in
        if remain_flag then Xlist.fold chosen [ht,hcat,e,c] (fun chosen (ht2,hcat2,e2,c2) ->
          if ht = ht2 then
            try if (StringMap.find gfrank_kasia e2) < (StringMap.find gfrank_kasia e) then chosen else (ht2,hcat2,e2,c2) :: chosen
            with Not_found -> failwith ("disambiguate_gf: " ^ e ^ " or " ^ e2 ^ " not in gfrank")
          else
            let r = try StringMap.find gfrank2_kasia (hcat^"_"^e^"_"^cat) with Not_found -> 10. in
            let r2 = try StringMap.find gfrank2_kasia (hcat2^"_"^e2^"_"^cat) with Not_found -> 10. in
            if r2 < r then chosen else (ht2,hcat2,e2,c2) :: chosen)
        else chosen)) in
    let c = Xlist.fold (List.tl chosen) (let _,_,_,c = List.hd chosen in c) (fun c2 (_,_,_,c) -> XTContext.union (c,c2)) in
    try XTContext.intersection (best_c,c) with XTContext.No_valid_choice -> best_c)
(* FIXME: dodać elimenację pro *)

let rec remove_context best_c = function
    Cons _ as t -> t
  | QCons _ as t -> t
  | LVar _ as t -> t
  | Compound(ids,l) ->
      let l = List.rev (Xlist.fold l [] (fun l (e,t) ->
        try (e, remove_context best_c t) :: l
        with Not_found -> l)) in
      Compound(ids,l)
  | Set(ids,l) ->
      let l = List.rev (Xlist.fold l [] (fun l t ->
        try (remove_context best_c t) :: l
        with Not_found -> l)) in
      if l = [] then raise Not_found else Set(ids,l)
  | Coordination(ids,l,l2) ->
      let l = List.rev (Xlist.fold l [] (fun l t ->
        try (remove_context best_c t) :: l
        with Not_found -> l)) in
      if l = [] then raise Not_found else
      Coordination(ids, l, Xlist.map l2 (fun (e,t) -> e, remove_context best_c t))
  | Loop _ as t -> t
  | Context l ->
      let l = List.rev (Xlist.fold l [] (fun l (c,t) ->
        try
          (XTContext.intersection (best_c,c), remove_context best_c t) :: l
        with XTContext.No_valid_choice -> l)) in
      (match l with
         [] -> raise Not_found
       | [c,t] -> if XTContext.equal (best_c,c) then t else Context[c,t]
       | _ -> Context l)

let rec remove_pros model_size best_c = function
    c :: l ->
       let best_c = try XTContext.difference model_size (best_c,c) with XTContext.No_valid_choice -> best_c in
       remove_pros model_size best_c l
  | [] -> best_c

let disambiguate model_size = function
    Context [] -> Context []
  | tree ->
  let tree2 = pred_of_ptype tree in
  let l = get_alternatives [] "root" "00" "root" "ROOT" CEmpty tree2 in
  let pros = Xlist.fold l [] (fun l (ht,hi,hcat,e,c,t,i,cat) ->
    if t = "pro" && c <> CEmpty then c :: l else l) in
  let map = Xlist.fold l StringMap.empty (fun map (ht,hi,hcat,e,c,t,i,cat) ->
    StringMap.add_inc map t [ht,hi,hcat,e,c,i,cat] (fun l -> (ht,hi,hcat,e,c,i,cat) :: l)) in
  let map = StringMap.fold map StringMap.empty (fun map t l ->
    let il = StringSet.to_list (Xlist.fold l StringSet.empty (fun set (ht,hi,hcat,e,c,i,cat) -> if i <> "" then StringSet.add set i else set)) in
    Xlist.fold l map (fun map (ht,hi,hcat,e,c,i,cat) ->
      let il = if i = "" then il else [i] in
      Xlist.fold il map (fun map i ->
        StringMap.add_inc map (t ^ "#" ^ i) (simplify_cat cat,[ht,hi,hcat,e,c]) (fun (cat,l) -> cat,(ht,hi,hcat,e,c) :: l)))) in
  let map = StringMap.map map (fun (cat,l) ->
    let map = Xlist.fold l StringMap.empty (fun map (ht,hi,hcat,e,c) ->
      StringMap.add_inc map ht [hi,hcat,e,c] (fun l -> (hi,hcat,e,c) :: l)) in
    let map = StringMap.fold map StringMap.empty (fun map ht l ->
      let hil = StringSet.to_list (Xlist.fold l StringSet.empty (fun set (hi,hcat,e,c) -> if hi <> "" then StringSet.add set hi else set)) in
      Xlist.fold l map (fun map (hi,hcat,e,c) ->
        let hil = if hi = "" then hil else [hi] in
        Xlist.fold hil map (fun map hi ->
          StringMap.add_inc map (ht ^ "#" ^ hi) (simplify_cat hcat,[e,c]) (fun (hcat,l) -> hcat,(e,c) :: l)))) in
    cat,map) in
  let map = StringMap.map map (fun (cat,map) ->
    cat, StringMap.map map (fun (hcat,l) ->
      hcat,Xlist.fold l StringMap.empty (fun map (e,c) ->
        StringMap.add_inc map e c (fun c2 -> XTContext.union (c,c2))))) in
  let map = remove_unambiguous map in
  File.file_out_append "results/dism.txt" (fun file ->
    Printf.fprintf file "---------------\n";
    Printf.fprintf file "pro: %s\n" (String.concat " " (Xlist.map pros XTStringOf.context_term));
    StringMap.iter map (fun t (cat,map) ->
      Printf.fprintf file "%s %s\n" t cat;
      StringMap.iter map (fun ht (hcat,map) ->
        Printf.fprintf file "  %s %s\n" ht hcat;
          StringMap.iter map (fun e c ->
            Printf.fprintf file "    %s %s\n" e (XTStringOf.context_term c)))));
  let best_c = disambiguate_gf map in
(*   let best_c = remove_pros model_size best_c pros in *)
(*   File.file_out_append "results/dism.txt" (fun file -> Printf.fprintf file "best_c=%s\n" (XTStringOf.context_term best_c)); *)
  let tree = remove_context best_c tree in
  tree
