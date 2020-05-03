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

open Xstd
open Printf
open ENIAMtokenizerTypes

let rec flatten_tokens rev_variants = function
  | [] -> rev_variants
  | Token t :: l -> flatten_tokens (Xlist.map rev_variants (fun rev_variant -> Token t :: rev_variant)) l
  | Seq seq :: l -> flatten_tokens rev_variants (seq @ l)
  | Variant variants :: l -> flatten_tokens (List.flatten (Xlist.map variants (fun variant -> flatten_tokens rev_variants [variant]))) l

let rec normalize_tokens rev = function
    [] -> List.rev rev
  | Token t :: l -> normalize_tokens (Token t :: rev) l
  | Seq seq :: l -> normalize_tokens rev (seq @ l)
  | Variant[t] :: l -> normalize_tokens rev (t :: l)
  | Variant variants :: l ->
      let variants = flatten_tokens [[]] [Variant variants] in
      let variants = Xlist.map variants (fun rev_seq ->
        match List.rev rev_seq with
          [] -> failwith "normalize_tokens"
        | [t] -> t
        | seq -> Seq seq) in
      let t = match variants with
          [] -> failwith "normalize_tokens"
        | [t] -> t
        | variants -> Variant variants in
      normalize_tokens (t :: rev) l

let concat_orths l =
  String.concat "" (Xlist.map l (fun t -> t.orth))

let concat_orths2 l =
  String.concat "" (Xlist.map l (fun t -> ENIAMtokens.get_orth t.token))

let concat_intnum = function
    [{token=Dig(v4,_)};_;{token=Dig(v3,_)};_;{token=Dig(v2,_)};_;{token=Dig(v1,_)}] -> v4^v3^v2^v1
  | [{token=Dig(v3,_)};_;{token=Dig(v2,_)};_;{token=Dig(v1,_)}] -> v3^v2^v1
  | [{token=Dig(v2,_)};_;{token=Dig(v1,_)}] -> v2^v1
  | [{token=Dig(v1,_)}] -> v1
  | _ -> failwith "concat_intnum"

let dig_value t =
  match t.token with
    Dig(v,_) -> v
  | _ -> failwith "dig_value"

let rec make_tys_rec n = function
    Dig(v,"intnum") -> Dig(v ^ String.make n '0',"intnum")
  | Dig(v,"realnum") ->
     (* print_endline ("make_tys_rec: v='" ^ v ^ "'"); *)
     let a,b = match Xstring.split "," v with [a;b] -> a,b | [a] -> a,"" | _ -> failwith "make_tys_rec" in
     (* print_endline ("make_tys_rec: '" ^ a ^ "' '" ^ b ^ "'"); *)
     let a = if a = "0" then "" else if a = "-0" then "-" else a in
     if String.length b > n then Dig(a ^ String.sub b 0 n ^ "," ^ String.sub b n (String.length b - n),"realnum") else
     Dig(a ^ b ^ String.make (n-String.length b) '0',"intnum")
  | Compound("intnum-interval",[x;y]) -> Compound("intnum-interval",[make_tys_rec n x; make_tys_rec n y])
  | Compound("realnum-interval",[x;y]) -> Compound("realnum-interval",[make_tys_rec n x; make_tys_rec n y])
  | _ -> failwith "make_tys_rec"

let make_tys n t =
  make_tys_rec n t.token

let digit_patterns1 = [ (* FIXME: problem z nadmiarowymi interpretacjami - trzeba uwzględnić w preprocesingu brak spacji - albo w dezambiguacji *)
  [D "dig"; S "."; D "dig"; S "."; D "dig"; S "."; D "dig"; S "."; D "dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"obj-id",[[]](*,["obj-id"]*)));
  [D "dig"; S "."; D "dig"; S "."; D "dig"; S "."; D "dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"obj-id",[[]](*,["obj-id"]*)));
  [D "dig"; S "."; D "dig"; S "."; D "dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"obj-id",[[]](*,["obj-id"]*)));
  [D "dig"; S "."; D "dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"obj-id",[[]](*,["obj-id"]*)));
(*   [D "dig"], "obj-id"; *)
  [D "pref3dig"; S "."; D "3dig"; S "."; D "3dig"; S "."; D "3dig"], (fun tokens -> Dig(concat_intnum tokens,"intnum"));
  [D "pref3dig"; S "."; D "3dig"; S "."; D "3dig"], (fun tokens -> Dig(concat_intnum tokens,"intnum"));
  [D "pref3dig"; S "."; D "3dig"], (fun tokens -> Dig(concat_intnum tokens,"intnum"));
  [D "pref3dig"; S " "; D "3dig"; S " "; D "3dig"; S " "; D "3dig"], (fun tokens -> Dig(concat_intnum tokens,"intnum"));
  [D "pref3dig"; S " "; D "3dig"; S " "; D "3dig"], (fun tokens -> Dig(concat_intnum tokens,"intnum"));
  [D "pref3dig"; S " "; D "3dig"], (fun tokens -> Dig(concat_intnum tokens,"intnum"));
  (* [D "intnum"; S "."], (function [token;_] -> Dig(concat_intnum [token],"ordnum") | _ -> failwith "digit_patterns1"); *) (* to zagłusza inne wzorce *)
  [D "day"; S "."; D "month"; S "."; D "year"], (function [day;_;month;_;year] -> Compound("date",[day.token;month.token;year.token]) | _ -> failwith "digit_patterns2");
  [D "day"; S "."; RD "month"; S "."; D "year"], (function [day;_;month;_;year] -> Compound("date",[day.token;month.token;year.token]) | _ -> failwith "digit_patterns3");
  [D "day"; S " "; RD "month"; S " "; D "year"], (function [day;_;month;_;year] -> Compound("date",[day.token;month.token;year.token]) | _ -> failwith "digit_patterns3");
  [D "day"; S "."; D "month"; S "."; D "2dig"], (function [day;_;month;_;year] -> Compound("date",[day.token;month.token;year.token]) | _ -> failwith "digit_patterns2");
  [D "day"; S "."; RD "month"; S "."; D "2dig"], (function [day;_;month;_;year] -> Compound("date",[day.token;month.token;year.token]) | _ -> failwith "digit_patterns3");
  (* [D "day"; S "."; D "month"; S "."], (function [day;_;month;_] -> Compound("day-month",[day.token;month.token]) | _ -> failwith "digit_patterns4"); *) (* to zagłusza inne wzorce *)
  [D "day"; S "."; D "month"], (function [day;_;month] -> Compound("day-month",[day.token;month.token]) | _ -> failwith "digit_patterns4");
  [D "hour"; S "."; D "minute"], (function [hour;_;minute] -> Compound("hour-minute",[hour.token;minute.token]) | _ -> failwith "digit_patterns5");
  [D "hour"; I ":"; D "minute"], (function [hour;_;minute] -> Compound("hour-minute",[hour.token;minute.token]) | _ -> failwith "digit_patterns6");
  [D "intnum"; I ":"; D "intnum"], (function [x;_;y] -> Compound("match-result",[x.token;y.token]) | _ -> failwith "digit_patterns7");
  [D "3dig"; I "-"; D "3dig"; I "-"; D "3dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "3dig"; S " "; D "3dig"; S " "; D "3dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "3dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "3dig"; S " "; D "2dig"; S " "; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "2dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "2dig"; S " "; D "2dig"; S " "; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "3dig"; S " "; D "3dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [O "0"; I "-"; D "2dig"; O " "; D "3dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [O "0"; I "-"; D "2dig"; O " "; D "2dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [I "("; O "0"; I "-"; D "2dig"; I ")"; O " "; D "3dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [I "("; O "0"; I "-"; D "2dig"; I ")"; O " "; D "2dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [I "("; D "3dig"; I ")"; O " "; D "3dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [I "("; D "3dig"; I ")"; O " "; D "2dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [I "("; D "3dig"; I ")"; O " "; D "3dig"; S " "; D "2dig"; S " "; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [I "("; D "3dig"; I ")"; O " "; D "2dig"; S " "; D "2dig"; S " "; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [O "0"; I "-"; D "2dig"; I "-"; D "2dig"; I "-"; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [O "0"; I "-"; D "3dig"; I "-"; D "2dig"; I "-"; D "3dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "3dig"; S " "; D "3dig"; S " "; D "2dig"; S " "; D "2dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "3dig"; S " "; D "3dig"; S " "; D "4dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
(* [D "year"; SL], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; S " "; SL2], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; I "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; I "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; I "/"; D "year"; SL], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; I "/"; D "year"; SL], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; I "/"; D "year"; I "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; I "/"; D "year"; I "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; I "/"; D "year"; SL; I "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)
  [D "year"; SL; I "/"; D "year"; SL; I "/"; D "year"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"building-number",[[]],["building-number"])); (* year - bo jest to dodatnia liczba całkowita *)*)
  (* [SL; I ")"], (fun tokens -> Dig(concat_orths tokens,"list-item")); *)
  [D "intnum"; S "."; D "dig"], (function [x;_;y] ->  Dig(dig_value x ^ "," ^ dig_value y,"realnum") | _ -> failwith "digit_patterns8");
  ] (* bez 1 i *2 *3 *4 mamy rec *) (* w morfeuszu zawsze num:pl?*)

let digit_patterns2 = [
  [D "intnum"; I ","; D "dig"], (function [x;_;y] ->  Dig(dig_value x ^ "," ^ dig_value y,"realnum") | _ -> failwith "digit_patterns8");
(*  [I "-"; D "intnum"; I ","; D "dig"], (function [_;x;_;y] ->  Dig("-" ^ dig_value x ^ "," ^ dig_value y,"realnum") | _ -> failwith "digit_patterns9");
  [I "-"; D "intnum"], (function [_;x] ->  Dig("-" ^ dig_value x,"realnum") | _ -> failwith "digit_patterns10");*)
  [I "’"; D "2dig"], (function [_;x] -> Dig("’" ^ dig_value x,"year") | _ -> failwith "digit_patterns12");
(*   [D "intnum"], "realnum"; *)
  ]

let compose_latek_lemma t interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ "-latek", interp)

let compose_latka_lemma t interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ "-latka", interp)

let compose_tka_lemma t interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ "-tka", interp)

let compose_latek_int_lemma t t2 interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ "-" ^ ENIAMtokens.get_orth t2.token ^ "-latek", interp)

let compose_latka_int_lemma t t2 interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ "-" ^ ENIAMtokens.get_orth t2.token ^ "-latka", interp)

let compose_lecie_lemma t interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ "-lecie", interp)

let compose_ordnum_lemma t interp =
  ENIAMtokens.make_lemma (ENIAMtokens.get_orth t.token ^ ".", interp)

let digit_patterns3 = [
  [I "-"; D "intnum"], (function [_;x] ->  Dig("-" ^ dig_value x,"intnum") | _ -> failwith "digit_patterns10");
  [I "-"; D "realnum"], (function [_;x] ->  Dig("-" ^ dig_value x,"realnum") | _ -> failwith "digit_patterns10");
  [D "2dig"; I "-"; D "3dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"postal-code",[[]](*,["postal-code"]*)));
  [D "3dig"; I "-"; D "3dig"], (fun tokens -> Lemma(*Proper*)(concat_orths tokens,"phone-number",[[]](*,["phone-number"]*)));
  [D "intnum"; I "-"; D "intnum"], (function [x;_;y] -> Compound("intnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns11");
  [D "realnum"; I "-"; D "realnum"], (function [x;_;y] -> Compound("realnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [D "intnum"; I "-"; D "realnum"], (function [x;_;y] -> Compound("realnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [D "realnum"; I "-"; D "intnum"], (function [x;_;y] -> Compound("realnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [C "date"; I "-"; C "date"], (function [x;_;y] -> Compound("date-interval",[x.token;y.token]) | _ -> failwith "digit_patterns13");
  [C "day-month"; I "-"; C "day-month"], (function [x;_;y] -> Compound("day-month-interval",[x.token;y.token]) | _ -> failwith "digit_patterns14");
  [D "day"; I "-"; D "day"], (function [x;_;y] -> Compound("day-interval",[x.token;y.token]) | _ -> failwith "digit_patterns15");
  [D "month"; I "-"; D "month"], (function [x;_;y] -> Compound("month-interval",[x.token;y.token]) | _ -> failwith "digit_patterns16");
  [RD "month"; I "-"; RD "month"], (function [x;_;y] -> Compound("month-interval",[x.token;y.token]) | _ -> failwith "digit_patterns17");
  [D "year"; I "-"; D "year"], (function [x;_;y] -> Compound("year-interval",[x.token;y.token]) | _ -> failwith "digit_patterns16");
  [D "year"; I "-"; D "2dig"], (function [x;_;y] -> Compound("year-interval",[x.token;y.token]) | _ -> failwith "digit_patterns16");
  [C "hour-minute"; I "-"; C "hour-minute"], (function [x;_;y] -> Compound("hour-minute-interval",[x.token;y.token]) | _ -> failwith "digit_patterns18");
  [D "hour"; I "-"; D "hour"], (function [x;_;y] -> Compound("hour-interval",[x.token;y.token]) | _ -> failwith "digit_patterns19");
  [D "minute"; I "-"; D "minute"], (function [x;_;y] -> Compound("minute-interval",[x.token;y.token]) | _ -> failwith "digit_patterns20");
  [RD "roman"; I "-"; RD "roman"], (function [x;_;y] -> Compound("roman-interval",[x.token;y.token]) | _ -> failwith "digit_patterns21");
  [D "intnum"; S " "; I "-"; S " "; D "intnum"], (function [x;_;_;_;y] -> Compound("intnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns11");
  [D "realnum"; S " "; I "-"; S " "; D "realnum"], (function [x;_;_;_;y] -> Compound("realnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [D "intnum"; S " "; I "-"; S " "; D "realnum"], (function [x;_;_;_;y] -> Compound("realnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [D "realnum"; S " "; I "-"; S " "; D "intnum"], (function [x;_;_;_;y] -> Compound("realnum-interval",[x.token;y.token]) | _ -> failwith "digit_patterns12"); (* FIXME: konflikt z liczbami ujemnymi *)
  [C "date"; S " "; I "-"; S " "; C "date"], (function [x;_;_;_;y] -> Compound("date-interval",[x.token;y.token]) | _ -> failwith "digit_patterns13");
  [C "day-month"; S " "; I "-"; S " "; C "day-month"], (function [x;_;_;_;y] -> Compound("day-month-interval",[x.token;y.token]) | _ -> failwith "digit_patterns14");
  [D "day"; S " "; I "-"; S " "; D "day"], (function [x;_;_;_;y] -> Compound("day-interval",[x.token;y.token]) | _ -> failwith "digit_patterns15");
  [D "month"; S " "; I "-"; S " "; D "month"], (function [x;_;_;_;y] -> Compound("month-interval",[x.token;y.token]) | _ -> failwith "digit_patterns16");
  [RD "month"; S " "; I "-"; S " "; RD "month"], (function [x;_;_;_;y] -> Compound("month-interval",[x.token;y.token]) | _ -> failwith "digit_patterns17");
  [D "year"; S " "; I "-"; S " "; D "year"], (function [x;_;_;_;y] -> Compound("year-interval",[x.token;y.token]) | _ -> failwith "digit_patterns16");
  [D "year"; S " "; I "-"; S " "; D "2dig"], (function [x;_;_;_;y] -> Compound("year-interval",[x.token;y.token]) | _ -> failwith "digit_patterns16");
  [C "hour-minute"; S " "; I "-"; S " "; C "hour-minute"], (function [x;_;_;_;y] -> Compound("hour-minute-interval",[x.token;y.token]) | _ -> failwith "digit_patterns18");
  [D "hour"; S " "; I "-"; S " "; D "hour"], (function [x;_;_;_;y] -> Compound("hour-interval",[x.token;y.token]) | _ -> failwith "digit_patterns19");
  [D "minute"; S " "; I "-"; S " "; D "minute"], (function [x;_;_;_;y] -> Compound("minute-interval",[x.token;y.token]) | _ -> failwith "digit_patterns20");
  [RD "roman"; S " "; I "-"; S " "; RD "roman"], (function [x;_;_;_;y] -> Compound("roman-interval",[x.token;y.token]) | _ -> failwith "digit_patterns21");
  [D "intnum"; I "-"; T "latek"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:nom:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latka"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkowi"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:dat:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkiem"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:inst:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latku"], (function [x;_;_] -> compose_latek_lemma x "subst:sg:loc.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkowie"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:nom.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latków"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkom"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:dat:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkami"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:inst:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkach"], (function [x;_;_] -> compose_latek_lemma x "subst:pl:loc:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latka"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:nom:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latki"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latce"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:dat.loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latkę"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:acc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latką"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latko"], (function [x;_;_] -> compose_latka_lemma x "subst:sg:voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latki"], (function [x;_;_] -> compose_latka_lemma x "subst:pl:nom.acc.voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "latek"], (function [x;_;_] -> compose_latka_lemma x "subst:pl:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latek"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:nom:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latka"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkowi"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:dat:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkiem"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:inst:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latku"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:sg:loc.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkowie"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:nom.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latków"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:gen.acc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkom"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:dat:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkami"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:inst:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkach"], (function [x;_;y;_;_] -> compose_latek_int_lemma x y "subst:pl:loc:m1.f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latka"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:nom:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latki"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latce"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:dat.loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latkę"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:acc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latką"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latko"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:sg:voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latki"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:pl:nom.acc.voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; D "intnum"; I "-"; T "latek"], (function [x;_;y;_;_] -> compose_latka_int_lemma x y "subst:pl:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "lecie"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:nom.acc.voc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "lecia"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:gen:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "leciu"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:dat.loc:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "leciem"], (function [x;_;_] -> compose_lecie_lemma x "subst:sg:inst:m1" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tka"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:nom:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tki"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tce"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:dat.loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkę"], (function [x;_;_] -> compose_tka_lemma x "subst:sg.acc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tką"], (function [x;_;_] -> compose_tka_lemma x "subst:sg.inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tko"], (function [x;_;_] -> compose_tka_lemma x "subst:sg:voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tki"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:nom.acc.voc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tek"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:gen:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkom"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:dat:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkami"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:inst:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tkach"], (function [x;_;_] -> compose_tka_lemma x "subst:pl:loc:f" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szy"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sze"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sza"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "si"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sze"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "szo"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ga"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "giego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "giemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "giej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "dzy"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gich"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "gimi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "go"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ci"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cia"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ciego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ciemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ciej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cią"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ci"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cie"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cich"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cim"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cimi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "cio"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ty"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "te"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ta"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "temu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ci"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "te"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "tymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "to"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sty"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ste"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sta"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stemu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ści"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ste"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "stymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "sto"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "my"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "me"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ma"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "memu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "me"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mo"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "y"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "i"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:m1.m2.m3:pos|adj:sg:acc:m3:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "e"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.acc.voc:n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "a"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:nom.voc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ego"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "go"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen:m1.m2.m3.n:pos|adj:sg:acc:m1.m2:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "emu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "mu"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:dat:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ej"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:gen.dat.loc:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "im"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:inst.loc:m1.m2.m3.n:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ą"], (function [x;_;_] -> compose_ordnum_lemma x "adj:sg:acc.inst:f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "i"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "y"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.voc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "e"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:nom.acc.voc:m2.m3.n.f:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ych"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ich"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:gen.loc:_:pos|adj:pl:acc:m1:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ym"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "im"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:dat:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "ymi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "imi"], (function [x;_;_] -> compose_ordnum_lemma x "adj:pl:inst:_:pos" | _ -> failwith "digit_patterns22");
  [D "intnum"; I "-"; T "o"], (function [x;_;_] -> compose_ordnum_lemma x "adja" | _ -> failwith "digit_patterns22");
  ]

let digit_patterns4 = [
  [D "intnum"; I "-"; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "intnum"; T "tys"; S "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "realnum"; I "-"; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "realnum"; S " "; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "realnum"; T "tys"; S "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; I "-"; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; S " "; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; T "tys"; S "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; I "-"; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; S " "; T "tys"; S "."], (function [x;_;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; T "tys"; S "."], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "intnum"; I "-"; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; T "mln"; S "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "realnum"; I "-"; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "realnum"; S " "; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "realnum"; T "mln"; S "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; I "-"; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; S " "; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; T "mln"; S "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; I "-"; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; S " "; T "mln"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; T "mln"; S "."], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; I "-"; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "intnum"; T "mld"; S "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "realnum"; I "-"; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "realnum"; S " "; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "realnum"; T "mld"; S "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; I "-"; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; S " "; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; T "mld"; S "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; I "-"; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; S " "; T "mld"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; T "mld"; S "."], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "intnum"; I "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "intnum"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "realnum"; I "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "realnum"; S " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "realnum"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; I "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; S " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; I "-"; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; S " "; T "tys"], (function [x;_;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; T "tys"], (function [x;_] -> make_tys 3 x | _ -> failwith "digit_patterns8");
  [D "intnum"; I "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "realnum"; I "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "realnum"; S " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "realnum"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; I "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; S " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; I "-"; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; S " "; T "mln"], (function [x;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; T "mln"], (function [x;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; I "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "intnum"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "realnum"; I "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "realnum"; S " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [D "realnum"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; I "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; S " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "intnum-interval"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; I "-"; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; S " "; T "mld"], (function [x;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
  [C "realnum-interval"; T "mld"], (function [x;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");
(*  [D "intnum"; S " "; T "miliony"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8"); FIXME: trzeba uwzględnić przypadek w lemacie
  [D "intnum"; S " "; T "milionów"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "milionach"; S "."], (function [x;_;_;_] -> make_tys 6 x | _ -> failwith "digit_patterns8");
  [D "intnum"; S " "; T "miliardów"; S "."], (function [x;_;_;_] -> make_tys 9 x | _ -> failwith "digit_patterns8");*)
  ]

(*let url_patterns1 = [
  [L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; D "dig"; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; D "dig"; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "pl"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "uk"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "cz"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "eu"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "org"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "com"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "net"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; S "."; L; S "."; L; S "."; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  [L; S "."; L; I "-"; L; S "."; O "gov"], (function l -> Dig(concat_orths2 l,"url"));
  ]

let url_patterns2 = [
  [L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "_"; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; L; S "."; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; D "dig"; S "."; L; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; D "intnum"; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; S "."; L; D "dig"; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [L; D "dig"; S "@"; D "url"], (function l -> Dig(concat_orths2 l,"email"));
  [O "http"; I ":"; I "/"; I "/"; D "url"], (function l -> Dig(concat_orths2 l,"url"));
  [O "https"; I ":"; I "/"; I "/"; D "url"], (function l -> Dig(concat_orths2 l,"url"));
  ]

let url_patterns3 = [
  [D "url"; I "/"], (function l -> Dig(concat_orths2 l,"url"));
  [D "url"; I "/"; L], (function l -> Dig(concat_orths2 l,"url"));
  [D "url"; I "/"; L; S "."; L], (function l -> Dig(concat_orths2 l,"url"));
]*)

let html_patterns = [
  [I "<"; L; I ">"], (function l -> Dig(concat_orths2 l,"html-tag"));
  [I "<"; I "/"; L; I ">"], (function l -> Dig(concat_orths2 l,"html-tag"));
  [I "<"; L; I "/"; I ">"], (function l -> Dig(concat_orths2 l,"html-tag"));
]


type matching = {
  prefix: tokens list;
  matched: token_env list;
  suffix: tokens list;
  pattern: pat list;
  command: token_env list -> token;
  command_abr: token_env list -> tokens list;
  }

let execute_command matching =
  let l = List.rev matching.matched in
  let len = Xlist.fold l 0 (fun len t -> t.len + len) in
  Seq((List.rev matching.prefix) @ [Token{empty_token_env with
    orth=concat_orths l;
    beg=(List.hd l).beg;
    len=len;
    next=(List.hd l).beg+len;
    token=matching.command l;
    weight=0.; (* FIXME: dodać wagi do konkretnych reguł i uwzględnić wagi maczowanych tokenów *)
    attrs=ENIAMtokens.merge_attrs l}] @ matching.suffix)

let execute_abr_command matching =
  let l = List.rev matching.matched in
  Seq((List.rev matching.prefix) @ (matching.command_abr l) @ matching.suffix)

let match_token = function
    D cat, Dig(_,cat2) -> cat = cat2
  | C s, Compound(s2,_) -> s = s2
  | S s, Symbol s2 -> s = s2
  | RD cat, RomanDig(_,cat2) -> cat = cat2
  | O pat, Dig(s,"dig") -> pat = s
  | O pat, Interp s -> pat = s
  | O pat, SmallLetter(uc,lc) -> pat = lc
  | O pat, CapLetter(uc,lc) -> pat = uc
  | O pat, AllSmall(uc,fc,lc) -> pat = lc
  | O pat, AllCap(uc,fc,lc) -> pat = uc
  | O pat, FirstCap(uc,fc,lc) -> pat = fc
  | O pat, SomeCap(uc,orth,lc) -> pat = orth
  | T pat, Dig(s,"dig") -> pat = s
  | T pat, Interp s -> pat = s
  | T pat, SmallLetter(uc,lc) -> pat = uc || pat = lc (* FIXME: kwestia wielkości liter *)
  | T pat, CapLetter(uc,lc) -> pat = uc || pat = lc
  | T pat, AllSmall(uc,fc,lc) -> pat = uc || pat = fc || pat = lc
  | T pat, AllCap(uc,fc,lc) -> pat = uc || pat = fc || pat = lc
  | T pat, FirstCap(uc,fc,lc) -> pat = uc || pat = fc || pat = lc
  | T pat, SomeCap(uc,orth,lc) -> pat = uc || pat = orth || pat = lc
  | L, SmallLetter _ -> true
  | L, CapLetter _ -> true
  | L, AllSmall _ -> true
  | L, AllCap _ -> true
  | L, FirstCap _ -> true
  | L, SomeCap _ -> true
  | CL, CapLetter _ -> true
  | CL, AllCap _ -> true
  | CL, SomeCap _ -> true
  | SL, SmallLetter _ -> true
  (* | SL2, SmallLetter x -> x <> "o" && x <> "w" (* FIXME !!! *) *)
  | SL, CapLetter _ -> true
  | I pat, Interp s -> pat = s
  | _ -> false

let rec find_first_token matching pat = function
    Token t -> if match_token (pat,t.token) then [{matching with matched = t :: matching.matched}] else []
  | Seq l -> Xlist.map (find_first_token matching pat (List.hd (List.rev l))) (fun matching -> {matching with prefix = matching.prefix @ (List.tl (List.rev l))})
  | Variant l -> List.flatten (Xlist.map l (find_first_token matching pat))

let rec find_middle_token matching pat = function
    Token t -> if match_token (pat,t.token) then [{matching with matched = t :: matching.matched}] else []
  | Seq _ -> []
  | Variant l -> List.flatten (Xlist.map l (find_middle_token matching pat))

let rec find_last_token matching pat = function
    Token t -> if match_token (pat,t.token) then [{matching with matched = t :: matching.matched}] else []
  | Seq l -> Xlist.map (find_last_token matching pat (List.hd l)) (fun matching -> {matching with suffix = matching.suffix @ (List.tl l)})
  | Variant l -> List.flatten (Xlist.map l (find_last_token matching pat))

let rec find_pattern_tail matchings = function
    [] -> raise Not_found
  | token :: l ->
      let found,finished = Xlist.fold matchings ([],[]) (fun (found,finished) matching ->
        match matching.pattern with
          [pat] -> found, (find_last_token {matching with pattern=[]} pat token) @ finished
        | pat :: pattern -> (find_middle_token {matching with pattern=pattern} pat token) @ found, finished
        | _ -> failwith "find_pattern: ni") in
      (try
        if found = [] then raise Not_found else
        find_pattern_tail found l
      with Not_found ->
        let finished = List.flatten (Xlist.map finished (fun matching -> try [execute_command matching] with Not_found -> [])) in
        if finished = [] then raise Not_found else Variant finished,l)

(* wzorce nie mogą mieć długości 1 *)
let rec find_pattern matchings rev = function
    token :: l ->
      let found = Xlist.fold matchings [] (fun found matching ->
        match matching.pattern with
          pat :: pattern -> (find_first_token {matching with pattern=pattern} pat token) @ found
        | [] -> failwith "find_pattern: empty pattern") in
      if found = [] then find_pattern matchings (token :: rev) l else
      (try
        let token,l = find_pattern_tail found l in
        find_pattern matchings (token :: rev) l
      with Not_found -> find_pattern matchings (token :: rev) l)
  | [] -> List.rev rev

let find_patterns patterns tokens =
  find_pattern (Xlist.map patterns (fun (pattern,command) ->
    {prefix=[]; matched=[]; suffix=[]; pattern=pattern; command=command; command_abr=(fun _ -> [])})) [] tokens

let rec find_abr_pattern_tail matchings = function
    [] -> raise Not_found
  | token :: l ->
      (* print_endline ("find_abr_pattern_tail 1: " ^ ENIAMtokens.string_of_tokens 0 token); *)
      let found,finished = Xlist.fold matchings ([],[]) (fun (found,finished) matching ->
        match matching.pattern with
          [pat] -> (*print_endline ("find_abr_pattern_tail 2a:");*) (find_last_token {matching with pattern=[]} pat token) @ found, finished
        | pat :: pattern -> (*print_endline ("find_abr_pattern_tail 2b:");*) (find_middle_token {matching with pattern=pattern} pat token) @ found, finished
        | [] -> (*print_endline "find_abr_pattern_tail 2c: []";*) found, matching :: finished) in
      (try
        (* print_endline "find_abr_pattern_tail 3"; *)
        if found = [] then raise Not_found else
        find_abr_pattern_tail found l
      with Not_found ->
        (* print_endline "find_abr_pattern_tail 4"; *)
        let finished = List.flatten (Xlist.map finished (fun matching -> try [execute_abr_command matching] with Not_found -> [])) in
        if finished = [] then raise Not_found else Variant finished,token :: l)

let rec find_abr_pattern matchings rev = function
    token :: l ->
      (* print_endline ("find_abr_pattern 1: " ^ ENIAMtokens.string_of_tokens 0 token); *)
      let found = Xlist.fold matchings [] (fun found matching ->
        match matching.pattern with
          pat :: pattern -> (find_first_token {matching with pattern=pattern} pat token) @ found
        | [] -> failwith "find_abr_pattern: empty pattern") in
      if found = [] then find_abr_pattern matchings (token :: rev) l else
      (try
        let token,l = find_abr_pattern_tail found l in
        find_abr_pattern matchings (token :: rev) l
      with Not_found -> find_abr_pattern matchings (token :: rev) l)
  | [] -> List.rev rev

let find_abr_patterns patterns tokens =
  (* Xlist.iter tokens (fun token -> print_endline ("A " ^ ENIAMtokens.string_of_tokens 0 token)); *)
  let tokens = find_abr_pattern (Xlist.map patterns (fun (pattern,command) ->
    {prefix=[]; matched=[]; suffix=[]; pattern=pattern; command=(fun _ -> Symbol ""); command_abr=command})) [] tokens in
  (* Xlist.iter tokens (fun token -> print_endline ("B " ^ ENIAMtokens.string_of_tokens 0 token)); *)
  tokens


(*exception PatternFound

let query_beg_patterns = [
  [I "<query>";I "<sentence>"];
  [I "<query>";I "„s";I "<sentence>"];
  [I "<query>";I "<or>";I "<sentence>"];
  ]

let query_end_patterns = [
  [I "</sentence>";I "</query>"];
  [I "</sentence>";I "”s";I "</query>"];
  ]

let find_beg_pattern pattern tokens =
  try
    let _ = find_pattern_tail [{prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> raise PatternFound);
         command_abr=(fun _ -> [])}] tokens in false
  with PatternFound -> true | Not_found -> false

let replace_beg_pattern pattern command tokens =
  try
    let t,l = find_abr_pattern_tail [{prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> Symbol "");
         command_abr=command}] tokens in
    t :: l
  with Not_found -> failwith "replace_beg_pattern"

(* let s_beg i = {empty_token_env with beg=i;len=1;next=i+1; token=Interp "<sentence>"}
let c_beg i = {empty_token_env with beg=i;len=1;next=i+1; token=Interp "<clause>"} *)
let s_end i = Token{empty_token_env with beg=i;len=1;next=i+1; token=Interp "</sentence>"}
let c_end i = Token{empty_token_env with beg=i;len=1;next=i+1; token=Interp "</clause>"}

let add_sentence_beg = function
    [q;t] -> let next=t.next in [Token q;Token{t with len=t.len-2;next=next-2};ENIAMtokens.s_beg (next-2);ENIAMtokens.c_beg (next-1)]
  | [q] -> let next=q.next in [Token{q with len=q.len-2;next=next-2};ENIAMtokens.s_beg (next-2);ENIAMtokens.c_beg (next-1)]
  | _ -> failwith "add_sentence_beg"

let add_sentence_end = function
    [q;t] -> let beg=t.beg in [Token q;Token{t with len=t.len-2;beg=beg+2};s_end (beg+1);c_end beg]
  | [q] -> let beg=q.beg in [Token{q with len=q.len-2;beg=beg+2};s_end (beg+1);c_end beg]
  | _ -> failwith "add_sentence_end"

let rec revert_tokens = function
    Token t -> Token t
  | Seq l -> Seq(Xlist.rev_map l revert_tokens)
  | Variant l -> Variant(Xlist.map l revert_tokens)

let manage_query_boundaries tokens =
  (* let b =
    try
      let _ = find_pattern_tail (Xlist.map query_beg_patterns (fun pattern ->
        {prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> raise PatternFound);
         command_abr=(fun _ -> [])})) tokens in false
    with PatternFound -> true | Not_found -> false in
  (if b then print_endline "sentence beg found" else print_endline "sentence beg not found"); *)
  let tokens =
    if find_beg_pattern [I "<query>";I "„s"] tokens then
      if find_beg_pattern [I "<query>";I "„s";I "<sentence>"] tokens then tokens else
      replace_beg_pattern [I "<query>";I "„s"] add_sentence_beg tokens else
    if find_beg_pattern [I "<query>";I "<or>"] tokens then
      if find_beg_pattern [I "<query>";I "<or>";I "<sentence>"] tokens then tokens else
      replace_beg_pattern [I "<query>";I "<or>"] add_sentence_beg tokens else
    if find_beg_pattern [I "<query>";I "(s";I "<sentence>"] tokens then tokens else
    if find_beg_pattern [I "<query>";I "<sentence>"] tokens then tokens else
    replace_beg_pattern [I "<query>"] add_sentence_beg tokens in
  (* let b =
    try
      let _ = find_pattern (Xlist.map query_end_patterns (fun pattern ->
        {prefix=[]; matched=[]; suffix=[];
         pattern=pattern; command=(fun _ -> raise PatternFound);
         command_abr=(fun _ -> [])})) [] tokens in false
    with PatternFound -> true in
  (if b then print_endline "sentence end found" else print_endline "sentence end not found"); *)
  let tokens = Xlist.rev_map tokens revert_tokens in
  let tokens =
    if find_beg_pattern [I "</query>";I "</sentence>"] tokens then tokens else
    if find_beg_pattern [I "</query>";I "”s";I "</sentence>"] tokens then tokens else
    if find_beg_pattern [I "</query>";I "”s"] tokens then
      replace_beg_pattern [I "</query>";I "”s"] add_sentence_end tokens else
    if find_beg_pattern [I "</query>";I ")s"(*;I "</sentence>"*)] tokens then tokens else
    replace_beg_pattern [I "</query>"] add_sentence_end tokens in
  let tokens = Xlist.rev_map tokens revert_tokens in
  tokens*)


let find_replacement_patterns tokens =
  let tokens = find_patterns digit_patterns1 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns digit_patterns2 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns digit_patterns3 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns digit_patterns4 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns ENIAMacronyms.acronym_patterns tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns !ENIAMacronyms.mte_patterns tokens in
  let tokens = normalize_tokens [] tokens in
(*   Xlist.iter tokens (fun t -> print_endline (ENIAMtokens.string_of_tokens 0 t)); *)
(*   let tokens = find_patterns ENIAMacronyms.name_patterns tokens in *) (* Wyłączone ze względu na to, że są w konflikcie z MWE *)
(*   Xlist.iter tokens (fun t -> print_endline (ENIAMtokens.string_of_tokens 0 t)); *)
  let tokens = normalize_tokens [] tokens in
(*  let tokens = find_patterns url_patterns1 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns url_patterns2 tokens in
  let tokens = normalize_tokens [] tokens in
  let tokens = find_patterns url_patterns3 tokens in
  let tokens = normalize_tokens [] tokens in*)
  let tokens = find_patterns html_patterns tokens in
  let tokens = normalize_tokens [] tokens in
  (*   Xlist.iter tokens (fun t -> print_endline (ENIAMtokens.string_of_tokens 0 t)); *)
  tokens

let rec set_next_id n = function
    Token t -> Token{t with next=n}
  | Seq l ->
      (match List.rev l with
        t :: l -> Seq(List.rev ((set_next_id n t) :: l))
      | [] -> failwith "set_next_id n")
  | Variant l -> Variant(Xlist.map l (set_next_id n))

let rec remove_spaces rev = function
    [] -> List.rev rev
  | x :: Token{token=Symbol " "; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Symbol "\t"; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Symbol "\n"; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Symbol "\r"; next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("<b>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("</b>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("<i>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("</i>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("<u>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("</u>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | x :: Token{token=Dig("<br/>","html-tag"); next=n} ::  l -> remove_spaces rev ((set_next_id n x) :: l)
  | Token{token=Symbol " "} :: l -> remove_spaces rev l
  | Token{token=Symbol "\t"} :: l -> remove_spaces rev l
  | Token{token=Symbol "\n"} :: l -> remove_spaces rev l
  | Token{token=Symbol "\r"} :: l -> remove_spaces rev l
  | Token{token=Dig("<b>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Dig("</b>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Dig("<i>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Dig("</i>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Dig("<u>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Dig("</u>","html-tag")} :: l -> remove_spaces rev l
  | Token{token=Dig("<br/>","html-tag")} :: l -> remove_spaces rev l
  | x :: l -> remove_spaces (x :: rev) l

(*let create_sentence_end_beg i len next orth =
  Seq[Token{empty_token_env with beg=i;len=20;next=i+20;token=Interp "</clause>"};
      Token{empty_token_env with orth=orth;beg=i+20;len=20;next=i+40;token=Interp "</sentence>"};
      Token{empty_token_env with beg=i+40;len=20;next=i+60;token=Interp "<sentence>"};
      Token{empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"}]

let create_clause_end_beg i len next orth =
  Seq[Token{empty_token_env with beg=i;len=60;next=i+60;token=Interp "</clause>"};
      Token{empty_token_env with beg=i+60;len=len-60;next=next;token=Interp "<clause>"}]

let process_interpunction_token t = function
    Interp "." -> Variant [create_sentence_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp "," -> Variant [create_clause_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp ":" -> Variant [create_clause_end_beg t.beg t.len t.next t.orth; create_sentence_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp ";" -> Variant [create_sentence_end_beg t.beg t.len t.next t.orth; Token t]
  | Interp "¶" -> Variant [create_clause_end_beg t.beg t.len t.next t.orth; create_sentence_end_beg t.beg t.len t.next t.orth]
  | Interp "<query>" -> Seq[
      Token{empty_token_env with orth=t.orth; beg=t.beg;len=60;next=t.beg+60;token=Interp "<query>"};
      Token{empty_token_env with beg=t.beg+60;len=20;next=t.beg+80;token=Interp "<sentence>"};
      Token{empty_token_env with beg=t.beg+80;len=20;next=t.next;token=Interp "<clause>"}]
  | Interp "</query>" -> Seq[
      Token{empty_token_env with beg=t.beg;len=20;next=t.beg+20;token=Interp "</clause>"};
      Token{empty_token_env with beg=t.beg+20;len=20;next=t.beg+40;token=Interp "</sentence>"};
      Token{empty_token_env with orth=t.orth; beg=t.beg+40;len=60;next=t.next;token=Interp "</query>"}]
  | _ -> Token t

let rec process_interpunction rev = function
    [] -> List.rev rev
  | Token t :: l -> process_interpunction ((process_interpunction_token t t.token) :: rev) l
  | Seq seq :: l -> process_interpunction (Seq(process_interpunction [] seq) :: rev) l
  | Variant variants :: l ->
      process_interpunction (Variant(process_interpunction [] variants) :: rev) l*)
  
