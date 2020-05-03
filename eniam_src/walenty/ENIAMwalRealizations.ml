(*
 *  ENIAMwalenty, a converter for Polish Valence Dictionary "Walenty".
 *  Copyright (C) 2016-2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016-2017 Institute of Computer Science Polish Academy of Sciences
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

open ENIAMwalTypes
open Xstd

let rec expand_schema_morf expands = function
    (* PhraseAbbr(Advp "misc",[]) -> PhraseAbbr(Advp "misc",[Phrase AdvP])
  | PhraseAbbr(Advp "mod",[]) -> PhraseAbbr(Advp "mod",[Phrase AdvP]) *)
  | PhraseAbbr(abbr,[]) -> (try PhraseAbbr(abbr,AbbrMap.find expands abbr) with Not_found -> failwith "expand_schema_morf")
  | PhraseAbbr(abbr,morfs) -> PhraseAbbr(abbr,Xlist.map morfs (expand_schema_morf expands))
  | LexPhrase(pos_lex,(restr,schema)) -> LexPhrase(pos_lex,(restr,expand_schema expands schema))
  | morf -> morf

and expand_schema expands schema =
  Xlist.map schema (fun s ->
      {s with morfs=Xlist.map s.morfs (expand_schema_morf expands)})

let rec expand_subtypes_morf subtypes = function
    PhraseComp(comp_morf,(ctype,comps)) ->
      let comps = if comps = [] then (try CompMap.find subtypes ctype with Not_found -> failwith "expand_subtypes_schema") else comps in
      Xlist.map comps (fun comp -> Phrase(match comp_morf with
          Cp -> CP(ctype,comp)
        | Ncp case -> NCP(case,ctype,comp)
        | Prepncp(prep,case) -> PrepNCP(prep,case,ctype,comp)))
  | LexPhrase(pos_lex,(restr,schema)) -> [LexPhrase(pos_lex,(restr,expand_subtypes subtypes schema))]
  | PhraseAbbr(abbr,morfs) ->
    List.flatten (Xlist.map morfs (expand_subtypes_morf subtypes))
  | E Null -> [E(NP(Str));E(NCP(Str,CompTypeUndef,CompUndef));E(CP(CompTypeUndef,CompUndef)); E(Or)]
  | morf -> [morf]

and expand_subtypes subtypes schema =
  Xlist.map schema (fun s ->
    {s with morfs=List.flatten (Xlist.map s.morfs (expand_subtypes_morf subtypes))})

let expand_equivs_phrase equivs = function
  | PrepNP(prep,case) -> Xlist.map (try StringMap.find equivs prep with Not_found -> [prep]) (fun prep -> PrepNP(prep,case))
  | PrepAdjP(prep,case) -> Xlist.map (try StringMap.find equivs prep with Not_found -> [prep]) (fun prep -> PrepAdjP(prep,case))
  | PrepNumP(prep,case) -> Xlist.map (try StringMap.find equivs prep with Not_found -> [prep]) (fun prep -> PrepNumP(prep,case))
  | ComprepNP(prep)  -> Xlist.map (try StringMap.find equivs prep with Not_found -> [prep]) (fun prep -> ComprepNP(prep))
  | ComparP(prep)  -> Xlist.map (try StringMap.find equivs prep with Not_found -> [prep]) (fun prep -> ComparP(prep))
  | CP(ctype,Comp comp) -> Xlist.map (try StringMap.find equivs comp with Not_found -> [comp]) (fun comp -> CP(ctype,Comp comp))
  | NCP(case,ctype,Comp comp) -> Xlist.map (try StringMap.find equivs comp with Not_found -> [comp]) (fun comp -> NCP(case,ctype,Comp comp))
  | PrepNCP(prep,case,ctype,Comp comp) -> List.flatten (
      Xlist.map (try StringMap.find equivs comp with Not_found -> [comp]) (fun comp ->
        Xlist.map (try StringMap.find equivs prep with Not_found -> [prep]) (fun prep ->
          PrepNCP(prep,case,ctype,Comp comp))))
  | phrase -> [phrase]

let rec expand_equivs_lex equivs = function
    Lexeme s -> (try XOR(Xlist.map (StringMap.find equivs s) (fun s -> Lexeme s)) with Not_found -> Lexeme s)
  | ORconcat l -> ORconcat(Xlist.map l (expand_equivs_lex equivs))
  | ORcoord l -> ORcoord(Xlist.map l (expand_equivs_lex equivs))
  | XOR l -> XOR(Xlist.map l (expand_equivs_lex equivs))
  | Elexeme gender -> Elexeme gender

let rec expand_equivs_morf equivs = function
    Phrase phrase -> Xlist.map (expand_equivs_phrase equivs phrase) (fun phrase -> Phrase phrase)
  | E phrase -> Xlist.map (expand_equivs_phrase equivs phrase) (fun phrase -> E phrase)
  | LexPhrase(pos_lex,(restr,schema)) -> [LexPhrase(Xlist.map pos_lex (fun (pos,lex) -> pos, expand_equivs_lex equivs lex),(restr,expand_equivs_schema equivs schema))]
  | morf -> failwith ("expand_equivs_morf: " ^ ENIAMwalStringOf.morf morf)

and expand_equivs_schema equivs schema =
  Xlist.map schema (fun s ->
    {s with morfs=List.flatten (Xlist.map s.morfs (expand_equivs_morf equivs))})

(* UWAGA: aktualnie equivs nie są wstawiane do expands *)
let load_realizations (expands,subtypes,equivs) =
  let subtypes = Xlist.fold subtypes CompMap.empty (fun subtypes -> function
      "int",l -> CompMap.add subtypes Int (List.flatten (Xlist.map l (fun v -> snd(ENIAMwalTEI.parse_comp v))))
    | "rel",l -> CompMap.add subtypes Rel (List.flatten (Xlist.map l (fun v -> snd(ENIAMwalTEI.parse_comp v))))
    | _ -> failwith "load_realizations 1") in
  let equivs = Xlist.fold equivs StringMap.empty (fun equivs (k,l) -> StringMap.add equivs k (k :: l)) in
  let expands,compreps,adv_types = Xlist.fold expands (AbbrMap.empty,[],[]) (fun (expands, compreps, adv_types) (id,k,l) ->
    match k with
        Phrase(AdvP m) -> expands, compreps, (m,Xlist.map l (function
            LexPhrase([ADV (Grad "pos"),Lexeme s],(Natr,[])) -> s
          | _ -> failwith "load_realizations 3")) :: adv_types
      | PhraseAbbr(Nonch,[]) -> AbbrMap.add expands Nonch l, compreps, adv_types
      (* | PhraseAbbr(Xp m,[]) -> AbbrMap.add expands (Xp m) (List.flatten (Xlist.map l (function
             PhraseAbbr(Advp m,[]) -> (try AbbrMap.find expands (Advp m) with Not_found -> [PhraseAbbr(Advp m,[])]) (* zakładam, że advp się nie rozmnoży *)
          | morf -> [morf]))), compreps, adv_types *)
      | PhraseAbbr(Xp m,[]) -> AbbrMap.add expands (Xp m) l, compreps, adv_types
      | Phrase(ComprepNP s) -> expands, (s, l) :: compreps, adv_types
      | PhraseAbbr(Distrp,[]) -> AbbrMap.add expands Distrp l, compreps, adv_types
      | PhraseAbbr(Possp,[]) -> AbbrMap.add expands Possp l, compreps, adv_types
      | _ -> failwith "load_realizations 2") in
  let compreps = Xlist.map compreps (fun (s,morfs) ->
    s, List.flatten (List.flatten (Xlist.map morfs (fun morf -> Xlist.map (expand_subtypes_morf subtypes (expand_schema_morf expands morf)) (expand_equivs_morf equivs))))) in
  expands,compreps,subtypes,equivs,adv_types
