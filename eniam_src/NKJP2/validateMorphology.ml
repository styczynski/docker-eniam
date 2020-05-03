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
open ValidateTokenizer

let rec has_brev = function
    BrevLemma _ :: _ -> true
  | _ :: l -> has_brev l
  | [] -> false

let rec get_brev = function
    BrevLemma s :: _ -> s
  | _ :: l -> get_brev l
  | [] -> failwith "get_brev"

let rec add_ntoken stats = function
    Token t ->
      (try
        let nlemma,ncat,ninterp = get_ntoken t.attrs in
        StringQMap.add stats (nlemma ^ "\t" ^ ncat ^ "\t" ^ ENIAMtagset.render [ninterp])
      with Not_found -> stats)
  | Seq l -> Xlist.fold l stats add_ntoken
  | Variant l -> Xlist.fold l stats add_ntoken

let create_ntoken_list stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let paragraph,tokens = annotate name sentences in
      Xlist.fold tokens stats add_ntoken))

(* let rec select_interp = function (* przejście z m1 do m1.p1 *)
    "n" :: l,["n1"] :: ll -> ["n1"] :: (select_interp (l,ll))
  | "n" :: l,["n2"] :: ll -> ["n2"] :: (select_interp (l,ll))
  | "n" :: l,["p2"] :: ll -> ["p2"] :: (select_interp (l,ll))
  | "n" :: l,["p3"] :: ll -> ["p3"] :: (select_interp (l,ll))
  | "n" :: l,["n1";"n2"] :: ll -> ["n1";"n2"] :: (select_interp (l,ll))
  | "n" :: l,["m1";"m2";"m3";"f";"n1";"n2";"p1";"p2";"p3"] :: ll -> ["n1";"n2";"p2";"p3"] :: (select_interp (l,ll))
  | "n" :: l,["m1";"m2";"m3";"f";"n1";"n2";"p1";"p2"] :: ll -> ["n1";"n2";"p2"] :: (select_interp (l,ll))
  | "n" :: l,["m1";"m2";"m3";"n1";"n2"] :: ll -> ["n1";"n2"] :: (select_interp (l,ll))
  | "n" :: l,["m1";"m2";"m3";"f";"n1";"n2"] :: ll -> ["n1";"n2"] :: (select_interp (l,ll))
  | "n" :: l,["m2";"m3";"f";"n1";"n2";"p2";"p3"] :: ll -> ["n1";"n2";"p2";"p3"] :: (select_interp (l,ll))
  | "m1" :: l,["_"] :: ll -> ["m1"] :: (select_interp (l,ll))
  | "m2" :: l,["_"] :: ll -> ["m2"] :: (select_interp (l,ll))
  | "m3" :: l,["_"] :: ll -> ["m3"] :: (select_interp (l,ll))
  | "f" :: l,["_"] :: ll -> ["f"] :: (select_interp (l,ll))
  | "n" :: l,["_"] :: ll -> ["n1";"n2";"p2";"p3"] :: (select_interp (l,ll))
  | a :: l,al :: ll -> if Xlist.mem al a then [a] :: (select_interp (l,ll)) else raise Not_found
  | [],[] -> []
  | _ -> raise Not_found *)

let lowercase s = function
    AllSmall _ -> s
  | SmallLetter _ -> s
  | FirstCap(_,_,c,l) ->
      if Xstring.check_prefix c s then
        l ^ Xstring.cut_prefix c s
      else failwith ("lowercase: " ^ s ^ " " ^ c)
  | t -> failwith ("lowercase: " ^ ENIAMtokens.string_of_token t)

let lemmatize_string s =
  let l = Xunicode.classified_chars_of_utf8_string s in
  let l = ENIAMtokens.tokenize l in
  let l = ENIAMpatterns.normalize_tokens [] l in
  let l = match l with
      [Token {token=Interp "<query>"};Variant l;Token {token=Interp "</query>"}] -> l
    | [Token {token=Interp "<query>"};t;Token {token=Interp "</query>"}] -> [t]
    | _ -> failwith ("lemmatize_string 1: " ^ s ^ " " ^ String.concat " " (Xlist.map l (fun t -> ENIAMtokens.string_of_tokens_simple t))) in
  let l = Xlist.fold l [] (fun l -> function
        Token ({token=AllSmall _} as t) -> t :: l
      | Token ({token=SmallLetter _} as t) -> t :: l
      | Token ({token=SomeCap _} as t) -> t :: l
      | Token ({token=FirstCap _} as t) -> t :: l
      | Token ({token=AllCap _} as t) -> t :: l
      | Token ({token=CapLetter _} as t) -> t :: l
      | Token ({token=RomanDig _} as t) -> (*print_endline ("lemmatize_string: " ^ s);*) t :: l
      | Token ({token=Dig _} as t) -> (*print_endline ("lemmatize_string: " ^ s);*) t :: l
      | Token ({token=Proper _} as t) -> t :: l
      | Seq[Token {token=AllSmall _};Token {token=Lemma _}] -> l
      | Seq[Token {token=SmallLetter _};Token {token=Lemma _}] -> l
      | Seq[Token {token=FirstCap _};Token {token=Lemma _}] -> l
      | Seq[Token {token=CapLetter _};Token {token=Lemma _}] -> l
      | Seq[Token {token=SomeCap _};Token {token=Lemma _}] -> l
      | Seq[Token {token=AllSmall _};Token {token=Lemma _};Token {token=Lemma _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=AllSmall _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=SmallLetter _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=FirstCap _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=AllCap _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=CapLetter _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=RomanDig _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=Dig _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=AllSmall _};Token {token=Lemma _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=FirstCap _};Token {token=Lemma _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=SmallLetter _};Token {token=Lemma _}] -> l
      | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=CapLetter _};Token {token=Lemma _}] -> l
      (* | Seq[Token {token=Interp "<sentence>"};Token {token=Interp "<clause>"};Token {token=AllSmall _};Token {token=Lemma _};Token {token=Lemma _}] -> l *)
      | t -> failwith ("lemmatize_string 3: " ^ ENIAMtokens.string_of_tokens_simple t)) in
  if l = [] then failwith "lemmatize_string 3" else
  List.flatten (Xlist.map l ENIAMpaths.lemmatize_token)
  (* match l with
    [] -> failwith "lemmatize_string 2"
  | [t] -> t
  | _ -> Xlist.iter l (fun t -> print_endline (ENIAMtokens.string_of_tokens_simple t)); failwith "lemmatize_string 3" *)
    (* Xlist.iter l (fun t -> print_endline (ENIAMtokens.string_of_tokens_simple t));
    print_endline "";
    Token empty_token_env *)

let get_cat_interp = function
    "subst","subst",[n;c;["m1"]],[_;_;["m1"]] -> "subst",[n;c;["m1"]]
  | "subst","subst",[n;c;["m1"]],[_;_;["m1"];col] -> "subst",[n;c;["m1"];col]
  | "subst","subst",[n;c;["m2"]],[_;_;["m2"]] -> "subst",[n;c;["m2"]]
  | "subst","subst",[n;c;["m3"]],[_;_;["m3"]] -> "subst",[n;c;["m3"]]
  | "subst","subst",[n;c;["n"]],[_;_;["n"];col] -> "subst",[n;c;["n"];col]
  | "subst","subst",[n;c;["f"]],[_;_;["f"]] -> "subst",[n;c;["f"]]
  | "subst","subst",[n;c;g],[_;_;_] -> "subst",[n;c;g]
  | "subst","subst",[n;c;g],[_;_;_;_] -> "subst",[n;c;g]
  | "subst","adj",[n;c;g],_ -> "subst",[n;c;g]
  | "depr","subst",[["pl"];["nom"];["m2"]],[["sg"];["nom"];["m1"]] -> "depr",[["pl"];["nom"];["m2"]]
  | "depr","subst",[["pl"];["acc"];["m2"]],[["sg"];["nom"];["m1"]] -> "depr",[["pl"];["acc"];["m2"]]
  | "ppron3","ppron3",ninterp,[["sg"];["nom"];["m1";"m2";"m3"];["ter"];_;_] -> "ppron3",ninterp
  | "ppron12","ppron12",ninterp,[_;["nom"];_;_] -> "ppron12",ninterp
  | "numcol","num",ninterp,_ -> "num",ninterp
  | "num","num",ninterp,_ -> "num",ninterp (* na tym etapie nie da się skorygować błędów *)
  (* | "num","num",[["pl"];c;g;["rec"]],[["sg";"pl"];["nom";"gen";"acc"];["m1";"m2";"m3";"f";"n"];["rec"]] -> "num",[["pl"];c;g;["rec"]]
  | "num","num",[["pl"];c;["m2"];["rec"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["rec"];col] -> "num",[["pl"];c;["m2"];["rec"]]
  | "num","num",[["pl"];c;["m3"];["rec"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["rec"];col] -> "num",[["pl"];c;["m3"];["rec"]]
  | "num","num",[["pl"];c;["f"];["rec"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["rec"];col] -> "num",[["pl"];c;["f"];["rec"]]
  | "num","num",[["pl"];c;["n"];["rec"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["rec"];col] -> "num",[["pl"];c;["n"];["rec"];col]
  | "num","num",[["pl"];c;["m1"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["congr"];col] -> "num",[["pl"];c;["m1"];["congr"]]
  | "num","num",[["pl"];c;["m2"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["congr"];col] -> "num",[["pl"];c;["m2"];["congr"]]
  | "num","num",[["pl"];c;["m3"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["congr"];col] -> "num",[["pl"];c;["m3"];["congr"]]
  | "num","num",[["pl"];c;["f"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["congr"];col] -> "num",[["pl"];c;["f"];["congr"]]
  | "num","num",[["pl"];c;["n"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"f";"n"];["congr"];col] -> "num",[["pl"];c;["n"];["congr"];col]
  | "num","num",[["pl"];c;["m2"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"n"];["congr"];col] -> "num",[["pl"];c;["m2"];["congr"]]
  | "num","num",[["pl"];c;["m3"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"n"];["congr"];col] -> "num",[["pl"];c;["m3"];["congr"]]
  | "num","num",[["pl"];c;["n"];["congr"]],[["pl"];["nom";"acc";"voc"];["m2";"m3";"n"];["congr"];col] -> "num",[["pl"];c;["n"];["congr"];col] *)
  | "siebie","siebie",[[c]],[["acc";"gen"]] -> "siebie",[[c]]
  | "adj","adj",ninterp,[["sg"];["nom";"voc"];["m1";"m2";"m3"];["pos"]] -> "adj",ninterp
  | "adj","adj",ninterp,[["sg";"pl"];["nom";"gen";"dat";"acc";"inst";"loc";"voc"];["m1";"m2";"m3";"f";"n"];["pos"]] -> "adj",ninterp
  | "adja","adj",ninterp,[["sg"];["nom";"voc"];["m1";"m2";"m3"];["pos"]] -> "adja",ninterp
  | "adjc","adj",ninterp,[["sg"];["nom";"voc"];["m1";"m2";"m3"];["pos"]] -> "adjc",ninterp
  | "adjp","adj",ninterp,[["sg"];["nom";"voc"];["m1";"m2";"m3"];["pos"]] -> "adjp",ninterp
  | "adj","adj",ninterp,[["sg"];["nom"];["m1";"m2";"m3"];["pos"]] -> "adj",ninterp
  | "adv","adv",[[g]],[["pos"]] -> "adv",[[g]]
  | "adv","adv",[],[["pos"]] -> "adv",[["pos"]]
  | "adv",_,ninterp,_ -> "adv",ninterp
  | "comp","comp",ninterp,interp -> if ninterp = interp then "comp",ninterp else raise Not_found
  | "conj","conj",ninterp,interp -> if ninterp = interp then "conj",ninterp else raise Not_found
  | "conj",_,ninterp,_ -> "conj",ninterp
  | "prep","prep",[c1;w],[c2;_] -> if c1 = c2 then "prep",[c1;w] else raise Not_found
  | "prep","prep",ninterp,interp -> if ninterp = interp then "prep",ninterp else raise Not_found
  | "qub","qub",ninterp,interp -> if ninterp = interp then "qub",ninterp else raise Not_found
  | "qub",_,ninterp,_ -> "qub",ninterp
  | "interj","interj",ninterp,interp -> if ninterp = interp then "interj",ninterp else raise Not_found
  | "interj",_,ninterp,_ -> "interj",ninterp
  | "burk","burk",ninterp,interp -> if ninterp = interp then "burk",ninterp else raise Not_found
  | "pred","pred",ninterp,interp -> if ninterp = interp then "pred",ninterp else raise Not_found
  | "fin","inf",[n;p;["imperf"]],[["imperf";"perf"]] -> "fin",[n;p;["imperf"]]
  | "fin","inf",[n;p;["imperf"]],[["imperf"]] -> "fin",[n;p;["imperf"]]
  | "fin","inf",[n;p;["perf"]],[["imperf";"perf"]] -> "fin",[n;p;["perf"]]
  | "fin","inf",[n;p;["perf"]],[["perf"]] -> "fin",[n;p;["perf"]]
  | "impt","inf",[n;p;["imperf"]],[["imperf";"perf"]] -> "impt",[n;p;["imperf"]]
  | "impt","inf",[n;p;["imperf"]],[["imperf"]] -> "impt",[n;p;["imperf"]]
  | "impt","inf",[n;p;["perf"]],[["imperf";"perf"]] -> "impt",[n;p;["perf"]]
  | "impt","inf",[n;p;["perf"]],[["perf"]] -> "impt",[n;p;["perf"]]
  | "bedzie","inf",[n;p;["imperf"]],[["imperf"]] -> "bedzie",[n;p;["imperf"]]
  | "aglt","inf",[n;p;["imperf"];w],[["imperf"]] -> "aglt",[n;p;["imperf"];w]
  | "inf","inf",[["imperf"]],[["imperf";"perf"]] -> "inf",[["imperf"]]
  | "inf","inf",[["imperf"]],[["imperf"]] -> "inf",[["imperf"]]
  | "inf","inf",[["perf"]],[["imperf";"perf"]] -> "inf",[["perf"]]
  | "inf","inf",[["perf"]],[["perf"]] -> "inf",[["perf"]]
  | "praet","inf",[n;g;["imperf"]],[["imperf";"perf"]] -> "praet",[n;g;["imperf"]]
  | "praet","inf",[n;g;["imperf"]],[["imperf"]] -> "praet",[n;g;["imperf"]]
  | "praet","inf",[n;g;["perf"]],[["imperf";"perf"]] -> "praet",[n;g;["perf"]]
  | "praet","inf",[n;g;["perf"]],[["perf"]] -> "praet",[n;g;["perf"]]
  | "praet","inf",[n;g;["imperf"];a],[["imperf";"perf"]] -> "praet",[n;g;["imperf"];a]
  | "praet","inf",[n;g;["imperf"];a],[["imperf"]] -> "praet",[n;g;["imperf"];a]
  | "praet","inf",[n;g;["perf"];a],[["imperf";"perf"]] -> "praet",[n;g;["perf"];a]
  | "praet","inf",[n;g;["perf"];a],[["perf"]] -> "praet",[n;g;["perf"];a]
  | "winien","winien",[n;g;["imperf"]],[_;_;["imperf"]] -> "winien",[n;g;["imperf"]]
  | "ppas","inf",[n;c;g;["imperf"];a],[["imperf";"perf"]] -> "ppas",[n;c;g;["imperf"];a]
  | "ppas","inf",[n;c;g;["imperf"];a],[["imperf"]] -> "ppas",[n;c;g;["imperf"];a]
  | "ppas","inf",[n;c;g;["perf"];a],[["imperf";"perf"]] -> "ppas",[n;c;g;["perf"];a]
  | "ppas","inf",[n;c;g;["perf"];a],[["perf"]] -> "ppas",[n;c;g;["perf"];a]
  | "pact","inf",[n;c;g;["imperf"];a],[["imperf";"perf"]] -> "pact",[n;c;g;["imperf"];a]
  | "pact","inf",[n;c;g;["imperf"];a],[["imperf"]] -> "pact",[n;c;g;["imperf"];a]
  | "pact","inf",[n;c;g;["perf"];a],[["imperf";"perf"]] -> "pact",[n;c;g;["perf"];a]
  | "pact","inf",[n;c;g;["perf"];a],[["perf"]] -> "pact",[n;c;g;["perf"];a]
  | "pant","inf",[["imperf"]],[["imperf";"perf"]] -> "pant",[["imperf"]]
  | "pant","inf",[["imperf"]],[["imperf"]] -> "pant",[["imperf"]]
  | "pant","inf",[["perf"]],[["imperf";"perf"]] -> "pant",[["perf"]]
  | "pant","inf",[["perf"]],[["perf"]] -> "pant",[["perf"]]
  | "pcon","inf",[["imperf"]],[["imperf";"perf"]] -> "pcon",[["imperf"]]
  | "pcon","inf",[["imperf"]],[["imperf"]] -> "pcon",[["imperf"]]
  | "pcon","inf",[["perf"]],[["imperf";"perf"]] -> "pcon",[["perf"]]
  | "pcon","inf",[["perf"]],[["perf"]] -> "pcon",[["perf"]]
  | "ger","inf",[n;c;g;["imperf"];a],[["imperf";"perf"]] -> "ger",[n;c;g;["imperf"];a]
  | "ger","inf",[n;c;g;["imperf"];a],[["imperf"]] -> "ger",[n;c;g;["imperf"];a]
  | "ger","inf",[n;c;g;["perf"];a],[["imperf";"perf"]] -> "ger",[n;c;g;["perf"];a]
  | "ger","inf",[n;c;g;["perf"];a],[["perf"]] -> "ger",[n;c;g;["perf"];a]
  | "imps","inf",[["imperf"]],[["imperf";"perf"]] -> "imps",[["imperf"]]
  | "imps","inf",[["imperf"]],[["imperf"]] -> "imps",[["imperf"]]
  | "imps","inf",[["perf"]],[["imperf";"perf"]] -> "imps",[["perf"]]
  | "imps","inf",[["perf"]],[["perf"]] -> "imps",[["perf"]]
  | _ -> raise Not_found

let get_lemma_cat_interp = function
    nlemma,lemma,"adj","ppas",[n;c;g;["pos"]],[["sg"];["nom";"voc"];["m1";"m2";"m3"];a;aff] -> lemma,"ppas",[n;c;g;a;aff]
  | nlemma,lemma,"adja","adja",[],[] -> lemma,"adja",[]
  | nlemma,lemma,"subst","subst",[["pl"];c;g],[["pl"];["nom";"voc"];_] -> lemma,"subst",[["pl"];c;g]
  (* | "5","5","adj","dig",ninterp,[] -> "piąty","adj",ninterp
  | "6","6","adj","dig",ninterp,[] -> "szósty","adj",ninterp *)
  (* | "adj","ppas",ninterp,interp -> print_endline (ENIAMtagset.render [ninterp] ^ " " ^ ENIAMtagset.render [interp]); raise Not_found *)
  | _ -> raise Not_found


let correct_nlemma = function
    "letnia  " -> "letnia"
  | "10minutowy" -> "minutowy"
  | "23-letni" -> "letni"
  | "40--letni" -> "letni"
  | "5minutowy" -> "minutowy"
  | "10-ta" -> (*"10."*)raise Not_found
  | "10-tej" -> (*"10."*)raise Not_found
  | "13-letni" -> "letni"
  | "itineraryjny " -> "itineraryjny"
  | "Składowy " -> "Składowy"
  | "tak " -> "tak"
  | "letni " -> "letni"
  | "Kaznodziey'a" -> raise Not_found
  | "Naczelna Rada Łowiecka" -> raise Not_found
  | "PR-owy" -> raise Not_found
  | "starać się" -> raise Not_found
  | "vis-à-vis" -> raise Not_found
  | "Ewangelia wg św. Jana" -> raise Not_found
  | "`a" -> raise Not_found
  | "6-piętrowy" -> "piętrowy"
  | "6-letni" -> "letni"
  | "5—lampowy" -> "lampowy"
  | "4-piętrowy" -> "piętrowy"
  | "3-centymetrowy" -> "centymetrowy"
  | "34-letni" -> "letni"
  | "18-ka" -> (*"18"*)raise Not_found
  | "185-osobowy" -> "osobowy"
  | "16-latek" -> raise Not_found
  | s -> s

let process_ntoken stats q nlemma ncat ninterp =
  try
    let nlemma = correct_nlemma nlemma in
    let nl = lemmatize_string nlemma in
    let nl2 = Xlist.fold nl [] (fun nl -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp nl (fun nl interp ->
            try
              if lemma = nlemma then
                let cat,interp = get_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: nl else
              let lemma,cat,interp = get_lemma_cat_interp (nlemma,lemma,ncat,cat,ninterp,interp) in
              (Lemma(lemma,cat,[interp])) :: nl
            with Not_found -> nl)
      | {token=Dig(_,"dig")} -> nl (* FIXME: todo *)
            (* (try
              let lemma,cat,interp = get_lemma_cat_interp (nlemma,lemma,ncat,cat,ninterp,interp) in
              (Lemma(lemma,cat,[interp])) :: nl
            with Not_found -> nl) *)
      | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: nl else nl
      | {token=Proper(lemma,cat,interp,_)} -> (*print_endline ("P " ^ nlemma);*) nl (* FIXME: todo *)
      | _ -> nl) in
    if nl2 = [] then StringQMap.add_val stats (ncat ^ " " ^ ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp])) ^ ": " ^ String.concat " " (Xlist.map nl (fun t -> ENIAMtokens.string_of_token t.token))) q
    else StringQMap.add_val stats "lemmatized" q
  with Not_found -> StringQMap.add_val stats "incorrect" q

let process_ntoken2 stats q name id_div orth beg paragraph nlemma ncat ninterp =
  try
    let nlemma = correct_nlemma nlemma in
    let nl = lemmatize_string nlemma in
    let nl2 = Xlist.fold nl [] (fun nl -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp nl (fun nl interp ->
            try
              if lemma = nlemma then
                let cat,interp = get_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: nl else
              let lemma,cat,interp = get_lemma_cat_interp (nlemma,lemma,ncat,cat,ninterp,interp) in
              (Lemma(lemma,cat,[interp])) :: nl
            with Not_found -> nl)
      | {token=Dig _} -> nl (* FIXME: todo *)
      | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: nl else nl
      | {token=Proper(lemma,cat,interp,_)} -> nl (* FIXME: todo *)
      | _ -> nl) in
    if nl2 = [] then
      StringQMap.add_val stats (ncat ^ " " ^ ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp])) ^ ": " ^ String.concat " " (Xlist.map nl (fun t -> ENIAMtokens.string_of_token t.token))
        ^ "\n" ^ name ^ " " ^ string_of_int id_div ^ " " ^ string_of_int beg ^ " " ^ orth ^ "\n" ^ paragraph) q
    else StringQMap.add_val stats "lemmatized" q
  with Not_found -> StringQMap.add_val stats "incorrect" q

let validate_ntoken stats q (nlemma,ncat,ninterp) =
  process_ntoken stats q nlemma ncat ninterp

let rec validate_ntoken_token name id_div paragraph stats = function
    Token t ->
      (try
        let nlemma,ncat,ninterp = get_ntoken t.attrs in
        process_ntoken2 stats 1 name id_div t.orth t.beg paragraph nlemma ncat ninterp
        (* print_endline (nlemma ^ "\t" ^ ncat ^ "\t" ^ ENIAMtagset.render [ninterp]);
        Printf.printf "%s\t%d\t%s\t%d\n" name id_div t.orth t.beg;
        print_endline paragraph;
        stats *)
      with Not_found -> stats)
  | Seq l -> Xlist.fold l stats (validate_ntoken_token name id_div paragraph)
  | Variant l -> Xlist.fold l stats (validate_ntoken_token name id_div paragraph)

let validate_ntoken_entry stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let paragraph,tokens = annotate name sentences in
      Xlist.fold tokens stats (validate_ntoken_token name id_div paragraph)))

let rec subset_list = function
    [],[] -> true
  | [x] :: l1, y :: l2 -> if Xlist.mem y x then subset_list (l1,l2) else false
  | _ -> false

let match_cat_interp = function
  | "subst","subst",[nn;nc;ng],[n;c;g;col] -> if subset_list ([nn;nc;ng],[n;c;g]) then "subst",[nn;nc;ng;col] else raise Not_found
(*  | "numcol","num",ninterp,_ -> "num",ninterp*)
  | "num","num",[nn;nc;["n"];na],[n;c;g;a;col] -> if subset_list ([nn;nc;["n"];na],[n;c;g;a]) then "num",[nn;nc;["n"];na;col] else raise Not_found
  | "num","num",[nn;nc;ng;na],[n;c;g;a;col] -> if subset_list ([nn;nc;ng;na],[n;c;g;a]) then "num",[nn;nc;ng;na] else raise Not_found
  | "adv","adv",[],[["pos"]] -> "adv",[["pos"]]
  | _ -> raise Not_found

let match_cat_interp_substgender = function
    "subst","subst",[nn;nc;ng],[n;c;_] -> if subset_list ([nn;nc],[n;c]) then "subst",[nn;nc;ng] else raise Not_found
  | "subst","subst",[nn;nc;ng],[n;c;_;_] -> if subset_list ([nn;nc],[n;c]) then "subst",[nn;nc;ng] else raise Not_found
  | _ -> raise Not_found

exception HasBrev
exception NoNtoken
exception LemmaNotMatched of string * string * string list list * token_env list
exception MultipleLemmaMatched of string * string * string list list * token list

let rec sort_uniq_rec rev = function
    [] -> rev
  | x :: y :: l -> if x = y then sort_uniq_rec rev (y :: l) else sort_uniq_rec (x :: rev) (y :: l)
  | [x] -> x :: rev

let sort_uniq l =
  match sort_uniq_rec [] (Xlist.sort l compare) with
    [Lemma(lemma1,"subst",[[n1;c1;["n"];["ncol"]]]);Lemma(lemma2,"subst",[[n2;c2;["n"];["col"]]])] as l ->
       if lemma1 = lemma2 && n1 = n2 && c1 = c2 then [Lemma(lemma1,"subst",[[n1;c1;["n"];["ncol";"col"]]])] else l
  | [Lemma("kląsknięcie","subst",[[["pl"];c1;["n"];["pt"]]]);Lemma("kląsknięcie","subst",[[["pl"];c2;["n"];["ncol"]]])] as l ->
       if c1 = c2 then [Lemma("kląsknięcie","subst",[[["pl"];c1;["n"];["pt"]]])] else l
  | [Lemma("wybrażenie","subst",[[["pl"];c1;["n"];["pt"]]]);Lemma("wybrażenie","subst",[[["pl"];c2;["n"];["ncol"]]])] as l ->
       if c1 = c2 then [Lemma("wybrażenie","subst",[[["pl"];c1;["n"];["pt"]]])] else l
  | [Lemma(lemma1,"subst",[[["pl"];c1;["n"];["pt"]]]);Lemma(lemma2,"subst",[[["pl"];c2;["n"];["ncol"]]])] as l ->
       (* print_endline lemma1; *)
       if lemma1 = lemma2 && c1 = c2 then [Lemma(lemma1,"subst",[[["pl"];c1;["n"];["pt"]]])] else l
  | l -> (*print_endline (String.concat " " (Xlist.map l (fun t -> ENIAMtokens.string_of_token t)));*) l

type t = TokenMatched | TokenLowercase | TokenBrev | TokenSubstGender | TokenDeviated

let match_lemmatize_simple t nlemma ncat ninterp =
  let l1 = ENIAMpaths.lemmatize_token t in
  let l2 = Xlist.fold l1 [] (fun l -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp l (fun l interp ->
            try
              if lemma = nlemma && cat = ncat && subset_list (ninterp,interp) then (Lemma(nlemma,ncat,[ninterp])) :: l else
              if lemma = nlemma then
                let cat,interp = match_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: l else l
            with Not_found -> l)
      | {token=Dig _} -> l (* FIXME: todo *)
      | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: l else l
      | {token=Proper(lemma,cat,interp,_)} -> l (* FIXME: todo *)
      | _ -> l) in
  match sort_uniq l2 with
    [] -> raise (LemmaNotMatched(nlemma,ncat,ninterp,l1))
  | [t] -> t, TokenMatched
  | _ -> raise (MultipleLemmaMatched(nlemma,ncat,ninterp,l2))

let match_lemmatize_lowercase t nlemma ncat ninterp =
  let t = match t.token with
    | FirstCap(s,lower,cl,ll) -> {t with token=AllSmall lower}
    | CapLetter(s,lower) -> {t with token=SmallLetter lower}
    | AllCap(_,a,b) -> {t with token=FirstCap(a,b,"","")} (* FIXME: to powinno być zdezambiguowane *)
    | _ -> t in
  let l = ENIAMpaths.lemmatize_token t in
  let l2 = Xlist.fold l [] (fun l -> function
      {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp l (fun l interp ->
            try
          if lemma = nlemma && cat = ncat && subset_list (ninterp,interp) then (Lemma(nlemma,ncat,[ninterp])) :: l else
              if lemma = nlemma then
                let cat,interp = match_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: l else l
            with Not_found -> l)
    | {token=Dig _} -> l (* FIXME: todo *)
    | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: l else l
    | {token=Proper(lemma,cat,interp,_)} -> l (* FIXME: todo *)
    | _ -> l) in
  match sort_uniq l2 with
    [] -> raise (LemmaNotMatched(nlemma,ncat,ninterp,l))
  | [t] -> t, TokenLowercase
  | _ -> raise (MultipleLemmaMatched(nlemma,ncat,ninterp,l2))

let match_lemmatize_substgender t nlemma ncat ninterp =
  let l1 = ENIAMpaths.lemmatize_token t in
  let l2 = Xlist.fold l1 [] (fun l -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp l (fun l interp ->
            try
              if lemma = nlemma then
                let cat,interp = match_cat_interp_substgender (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: l else l
            with Not_found -> l)
      | {token=Dig _} -> l (* FIXME: todo *)
      | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: l else l
      | {token=Proper(lemma,cat,interp,_)} -> l (* FIXME: todo *)
      | _ -> l) in
  match sort_uniq l2 with
    [] -> raise (LemmaNotMatched(nlemma,ncat,ninterp,l1))
  | [t] -> t, TokenSubstGender
  | _ -> raise (MultipleLemmaMatched(nlemma,ncat,ninterp,l2))

let match_lemmatize_deviated t nlemma ncat ninterp =
  let l1 = ENIAMpaths.lemmatize_token t in
  let nlemma = try correct_nlemma nlemma with Not_found -> raise (LemmaNotMatched(nlemma,ncat,ninterp,l1)) in
  let nl = lemmatize_string nlemma in
  let nl2 = Xlist.fold nl [] (fun nl -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp nl (fun nl interp ->
            try
              let lemma,cat,interp = get_lemma_cat_interp (nlemma,lemma,ncat,cat,ninterp,interp) in
              (lemma,cat,interp) :: nl
            with Not_found -> nl)
      | _ -> nl) in
  let l2 = Xlist.fold nl2 [] (fun l (nlemma,ncat,ninterp) ->
    Xlist.fold l1 l (fun l -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp l (fun l interp ->
            try
              if lemma = nlemma && cat = ncat && subset_list (ninterp,interp) then (Lemma(nlemma,ncat,[ninterp])) :: l else
              if lemma = nlemma then
                let cat,interp = match_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: l else l
            with Not_found -> l)
      | _ -> l)) in
  match sort_uniq l2 with
    [] -> raise (LemmaNotMatched(nlemma,ncat,ninterp,l1))
  | [t] -> t, TokenDeviated
  | _ -> raise (MultipleLemmaMatched(nlemma,ncat,ninterp,l2))

let rec match_lemmatize_rec t nlemma ncat ninterp f0 = function
    f :: l ->
       (try f t nlemma ncat ninterp
        with LemmaNotMatched _ -> match_lemmatize_rec t nlemma ncat ninterp f0 l)
  | [] -> f0 t nlemma ncat ninterp

let match_lemmatize (*stats q name id_div paragraph*) t =
  if has_brev t.attrs then raise HasBrev (*StringQMap.add_val stats "brev" q*)
(*    let nlemma = get_brev t.attrs in
      (let l = ENIAMpaths.lemmatize_token t in
      let l2 = Xlist.fold l [] (fun l -> function
            {token=Lemma(lemma,cat,interp)} ->
              Xlist.fold interp l (fun l interp ->
                try
                  if lemma = nlemma then (Lemma(nlemma,cat,[interp])) :: l else l
                with Not_found -> l)
          (* | {token=Dig _} -> l (* FIXME: todo *)
          | {token=RomanDig(_,"roman")} ->
              if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: l else l
          | {token=Proper(lemma,cat,interp,_)} -> l (* FIXME: todo *) *)
          | _ -> l) in
      match sort_uniq l2 with
        [] -> raise (LemmaNotMatched(nlemma,"BREV",[],l))
      | [t] -> t, TokenBrev
      | _ -> raise (MultipleLemmaMatched(nlemma,"BREV",[],l2)))*)
  else
  let nlemma,ncat,ninterp = try get_ntoken t.attrs with Not_found -> raise NoNtoken in
  match_lemmatize_rec t nlemma ncat ninterp match_lemmatize_simple
    [match_lemmatize_simple; match_lemmatize_lowercase; match_lemmatize_substgender; match_lemmatize_deviated]
  (* let ninterp = if ncat = "adv" && ninterp = [] then [["pos"]] else ninterp in *)
(*  let l1 = ENIAMpaths.lemmatize_token t in
  let l2 = Xlist.fold l1 [] (fun l -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp l (fun l interp ->
            try
              if lemma = nlemma && cat = ncat && subset_list (ninterp,interp) then (Lemma(nlemma,ncat,[ninterp])) :: l else
              if lemma = nlemma then
                let cat,interp = match_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: l else l
            with Not_found -> l)
      | {token=Dig _} -> l (* FIXME: todo *)
      | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: l else l
      | {token=Proper(lemma,cat,interp,_)} -> l (* FIXME: todo *)
      | _ -> l) in
  match sort_uniq l2 with
    [] -> (*raise (LemmaNotMatched(nlemma,ncat,ninterp,l))*)
lowercase
  | [t] -> t, TokenMatched
  | _ -> raise (MultipleLemmaMatched(nlemma,ncat,ninterp,l2))*)

(*  try
    let nlemma = correct_nlemma nlemma in
    let nl = lemmatize_string nlemma in
    let nl2 = Xlist.fold nl [] (fun nl -> function
        {token=Lemma(lemma,cat,interp)} ->
          Xlist.fold interp nl (fun nl interp ->
            try
              if lemma = nlemma then
                let cat,interp = get_cat_interp (ncat,cat,ninterp,interp) in
                (Lemma(lemma,cat,[interp])) :: nl else
              let lemma,cat,interp = get_lemma_cat_interp (nlemma,lemma,ncat,cat,ninterp,interp) in
              (Lemma(lemma,cat,[interp])) :: nl
            with Not_found -> nl)
      | {token=Dig _} -> nl (* FIXME: todo *)
      | {token=RomanDig(_,"roman")} ->
          if ncat = "adj" then (Lemma(nlemma,ncat,[ninterp])) :: nl else nl
      | {token=Proper(lemma,cat,interp,_)} -> nl (* FIXME: todo *)
      | _ -> nl) in
    if nl2 = [] then
      StringQMap.add_val stats (ncat ^ " " ^ ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp])) ^ ": " ^ String.concat " " (Xlist.map nl (fun t -> ENIAMtokens.string_of_token t.token))
        ^ "\n" ^ name ^ " " ^ string_of_int id_div ^ " " ^ string_of_int t.beg ^ " " ^ t.orth ^ "\n" ^ paragraph) q
    (* if nl2 = [] then StringQMap.add_val stats (ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp])) ^ ": " ^ String.concat " " (Xlist.map nl (fun t -> ENIAMtokens.string_of_token t.token))) q *)
    (* let l2 = Xlist.fold l [] (fun l2 t2 ->
      match t2.token with
        Lemma(lemma,cat,interp) -> if lemma = nlemma (*|| lemma = lowercase nlemma t.token*) then t2 :: l2 else l2
      (* | Proper(lemma,cat,interp,_) -> if lemma = nlemma || lemma = lowercase nlemma t.token then t2 :: l2 else l2 *)
      | _  -> l2) in
    if l2 = [] then StringQMap.add stats ("no lemma: " ^ t.orth ^ " " ^ nlemma) else *)
    else StringQMap.add_val stats "lemmatized" q
(*  let l3 = Xlist.fold l2 [] (fun l3 t ->
    match t.token with
      Lemma(lemma2,cat2,interp2) -> if cat = cat2 then t :: l3 else l3
    | Proper(lemma2,cat2,interp2,_) -> if cat = cat2 then t :: l3 else l3
    | _  -> t :: l3) in
  if l3 = [] then StringQMap.add stats ("no cat: " ^ t.orth ^ " " ^ lemma ^ " " ^ cat) else
  let l4 = Xlist.fold l3 [] (fun l4 t ->
    match t.token with
      Lemma(lemma2,cat2,interp2) ->
        let interp2 = Xlist.fold interp2 [] (fun interp2 interp3 -> try select_interp (interp,interp3) :: interp2 with Not_found -> interp2) in
        if interp2 = [] then l4 else (
        if Xlist.size interp2 > 1 then print_endline "match_lemmatize: multiple interp";
        (try {t with token=Lemma(lemma2,cat2,interp2)} :: l4 with Not_found -> l4))
    | Proper(lemma2,cat2,interp2,sense2) ->
        let interp2 = Xlist.fold interp2 [] (fun interp2 interp3 -> try select_interp (interp,interp3) :: interp2 with Not_found -> interp2) in
        if interp2 = [] then l4 else (
        if Xlist.size interp2 > 1 then print_endline "match_lemmatize: multiple interp";
        (try {t with token=Proper(lemma2,cat2,interp2,sense2)} :: l4 with Not_found -> l4))
    | _  -> t :: l4) in
  match l4 with
    [] -> StringQMap.add stats ("no interp: " ^ t.orth ^ " " ^ lemma ^ " " ^ cat ^ " " ^ String.concat ":" interp ^ "\n" ^ String.concat "\n" (Xlist.map l ENIAMtokens.string_of_token_env))
  (* | [{token=AllSmall _}] -> StringQMap.add stats ("no Lemma: " ^ t.orth ^ " " ^ lemma ^ " " ^ cat ^ " " ^ String.concat ":" interp ^ "\n" ^ String.concat "\n" (Xlist.map l ENIAMtokens.string_of_token_env)) *)
  | [{token=AllSmall _}] -> StringQMap.add stats "no Lemma"
  | [{token=FirstCap _}] -> StringQMap.add stats ("no Lemma 2: " ^ t.orth ^ " " ^ lemma ^ " " ^ cat ^ " " ^ String.concat ":" interp ^ "\n" ^ String.concat "\n" (Xlist.map l ENIAMtokens.string_of_token_env))
  (* | [{token=FirstCap _}] -> StringQMap.add stats "no Lemma2" *)
  | [{token=Lemma _};{token=AllSmall _}] -> stats
  | [{token=Lemma _};{token=SmallLetter _}] -> stats
  | [{token=Lemma _};{token=FirstCap _}] -> stats
  | l -> StringQMap.add stats ("multiple interp: " ^ t.orth ^ " " ^ lemma ^ " " ^ cat ^ "\n" ^ String.concat "\n" (Xlist.map l ENIAMtokens.string_of_token_env))*)
  with Not_found -> StringQMap.add_val stats "no ntoken/incorrect" q
  (* with Not_found -> StringQMap.add_val stats "no ntoken" q (*("no ntoken for: " ^ t.orth ^ " " ^ ENIAMtokens.string_of_token t.token)*) *)*)

let rec validate_token name id_div paragraph stats = function
    Token t ->
      (* if t.orth = "POWIŚLE" then Printf.printf "%s %d %s\n%s\n" name id_div paragraph (ENIAMtokens.string_of_token_env t); *)
      (try let _,f = match_lemmatize (*stats 1 name id_div paragraph*) t in
        match f with
          TokenMatched -> StringQMap.add stats "validated"
        | TokenLowercase -> StringQMap.add stats "validated as lowercase"
        | TokenBrev -> StringQMap.add stats "validated abbreviation"
        | TokenSubstGender -> StringQMap.add stats "validated substgender"
        | TokenDeviated -> StringQMap.add stats "validated deviated"
      with
        HasBrev -> StringQMap.add stats ("has brev: " ^ t.orth (*^ " " ^ lemma ^ " " ^ cat ^ "\n"*))
      (* | NoNtoken -> StringQMap.add stats ("no ntoken: " ^ t.orth (*^ " " ^ lemma ^ " " ^ cat ^ "\n"*)) *)
      | NoNtoken -> StringQMap.add stats "no ntoken"
      | LemmaNotMatched(nlemma,ncat,ninterp,l) ->
          (* StringQMap.add stats (Printf.sprintf "lemma not matched: %s %s : %s \n%s" t.orth (ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp]))) (String.concat " " (Xlist.map l (fun t -> ENIAMtokens.string_of_token t.token))) paragraph) *)
          StringQMap.add stats (Printf.sprintf "%s %s %s %d %s\n#%s\n#%s" ncat t.orth name id_div (ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp])))
          (String.concat " " (Xlist.map l (fun t -> ENIAMtokens.string_of_token t.token))) paragraph)
      | MultipleLemmaMatched(nlemma,ncat,ninterp,l) -> StringQMap.add stats (Printf.sprintf "multiple lemma matched: %s %s : %s" t.orth (ENIAMtokens.string_of_token (Lemma(nlemma,ncat,[ninterp]))) (String.concat " " (Xlist.map l (fun t -> ENIAMtokens.string_of_token t)))))
  | Seq l -> Xlist.fold l stats (validate_token name id_div paragraph)
  | Variant l -> Xlist.fold l stats (validate_token name id_div paragraph)

let validate_morphology stats name typ channel entries =
  prerr_endline name;
  Xlist.fold entries stats (fun stats (id_div,has_ne,paragraphs) ->
    Xlist.fold paragraphs stats (fun stats (paragraph,sentences) ->
      let paragraph,tokens = annotate name sentences in
      (* print_endline paragraph; *)
      (*let s = "W Specjalnym Ośrodku Szkolno-Wychowawczym" in
      if String.length paragraph >= String.length s && String.sub paragraph 0 (String.length s) = s then*)
        Xlist.fold tokens stats (validate_token name id_div paragraph)
      (*else stats*)))

let ntokens_filename = "results/ntokens.tab"

let parse_ninterp s =
  Xlist.map (Xstring.split ":" s) (fun s -> Xstring.split "\\." s)

let fold_ntokens ntokens_filename s f =
  File.fold_tab ntokens_filename s (fun s -> function
      [q;nlemma;ncat;ninterp] -> f s (int_of_string q) (nlemma,ncat,parse_ninterp ninterp)
    | l -> failwith ("fold_ntokens: " ^ String.concat "\t" l))

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
  "110-4-000000102";"KurierKwidzynski";
  (* "620-3-010001496"; *)
]

let _ =
  ENIAMtokenizer.initialize ();
  ENIAMinflexion.initialize ();
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] StringQMap.empty (fun stats (name,typ,channel,entries) ->
    create_ntoken_list stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    create_ntoken_list stats name typ channel entries) in *)
  (* let stats = fold_ntokens ntokens_filename StringQMap.empty validate_ntoken in *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate_ntoken_entry stats name typ channel entries) in *)
  (* let stats = ENIAM_NKJP.fold_selected ENIAM_NKJP.nkjp_path selection [] [] StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate_morphology stats name typ channel entries) in *)
  let stats = ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path StringQMap.empty (fun stats (name,typ,channel,entries) ->
    validate_morphology stats name typ channel entries) in
  let stats = StringQMap.fold stats [] (fun stats k v -> (v,k) :: stats) in
  Xlist.iter (Xlist.sort stats compare) (fun (v,k) -> Printf.printf "%d\t%s\n" v k);
  flush stdout;
  ignore(Sys.command "mpg123 \"../../Inne/gong/gong_00m_30s.mp3\"");
  ()
