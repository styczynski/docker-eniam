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

open Xstd

type opinion = Pewny | Potoczny | Watpliwy | Archaiczny | Zly | Wulgarny | Nieokreslony
             | Metaforyczny | Dziedzinowy | Sporadyczny | OpinionUndef
type negation = Negation | Aff | NegationUndef | NegationNA
type pred = PredTrue | PredFalse | PredUndef | PredNA
type aspect = Aspect of string | AspectUndef | AspectNA
type case = Case of string | Str | Part | CaseAgr | NomAgr | GenAgr | AllAgr | CaseUndef | AllUAgr | CaseUAgr
type comp = Comp of string | Zeby | Gdy | CompUndef
type comp_type = Int | Rel | CompTypeUndef
type number = Number of string | NumberUndef | NumberAgr
type gender = Gender of string | GenderUndef | GenderAgr | Genders of string list
type grad = Grad of string | GradUndef
type refl = ReflEmpty | ReflTrue | ReflFalse | ReflUndef
(* type acm = Acm of string | AcmUndef *)

type gf = SUBJ | OBJ | ARG | HEAD (* FIXME *)

type pos =
    SUBST of number * case
  | PPRON12 of number * case
  | PPRON3 of number * case
  | SIEBIE of case
  | PREP of case
  | NUM of case * gender (* acm*)
  | ADJ of number * case * gender * grad
  | ADV of grad
  | GER of number * case * gender * aspect * negation (** refl*)
  | PACT of number * case * gender * aspect * negation (* refl*)
  | PPAS of number * case * gender * aspect * negation
  | INF of aspect * negation (* refl*)
  | QUB
  | COMPAR
  | COMP of comp_type
  | PERS of (*number * gender * aspect * person * *)negation (* refl*)
  | FIXED

type phrase =
    NP of case
  | PrepNP of string * case
  | AdjP of case
  | PrepAdjP of string * case
  | NumP of case
  | PrepNumP of string * case
  | ComprepNP of string
  | ComparP of string (** case*)
  | CP of comp_type * comp
  | NCP of case * comp_type * comp
  | PrepNCP of string * case * comp_type * comp
  | InfP of aspect
  | AdvP of string
  | FixedP of string
  (* | Num of case (* acm*) *)
  | Or
  | Qub
  (* | Pro
  | ProNG *)
  | Null
  | GerP of case
  | PrepGerP of string * case
  | PpasP of case
  | PrepPpasP of string * case
  | PactP of case

type phrase_abbr =
    Xp of string
  | Nonch
  | Distrp
  | Possp

type phrase_comp =
    Cp
  | Ncp of case
  | Prepncp of string * case

type lex =
    Lexeme of string
  | ORconcat of lex list
  | ORcoord of lex list
  | XOR of lex list
  | Elexeme of gender

type restr = Natr | Ratr | Ratrs | Ratr1 | Atr | Atr1 | NoRestr

type sel_prefs =
    SynsetId of int
  | Predef of string
  | RelationArgId of string * int (* nazwa relacji * id argumentu ramy *)
  | RelationRole of string * string * string (* relacji * rola * atrybut roli *)

type position = {psn_id: int; gf: gf; role: string; role_attr: string; sel_prefs: sel_prefs list;
                 mode: string list; cr: string list; ce: string list; morfs: morf list}

and morf =
    Phrase of phrase
  | E of phrase
  | LexPhrase of (pos * lex) list * (restr * position list)
  | PhraseAbbr of phrase_abbr * morf list
  | PhraseComp of phrase_comp * (comp_type * comp list)
  | MorfId of int
  | SimpleLexArg of string * pos
  | LexArg of int * string * pos

let empty_position =
  {psn_id=(-1); gf=ARG; role=""; role_attr=""; mode=[]; sel_prefs=[]; cr=[]; ce=[]; morfs=[]}

type lex_record = {
  lex_argument: morf;
  lex_arguments: morf list;
  lex_lemma: lex;
  lex_numeral_lemma: lex;
  lex_mode: string list;
  lex_negation: negation;
  lex_degree: grad;
  lex_number: number;
  lex_reflex: refl;
  lex_gender: gender;
  lex_modification: restr * position list;
}

let empty_lex = {lex_argument=Phrase Null; lex_arguments=[]; lex_lemma=Lexeme "";
                 lex_numeral_lemma=Lexeme ""; lex_mode=[]; lex_negation=NegationUndef;
                 lex_degree=GradUndef; lex_number=NumberUndef; lex_reflex=ReflUndef;
                 lex_gender=GenderUndef; lex_modification = Natr,[]}

type meaning = {mng_id: int;
                name: string;
                variant: string;
                plwnluid: int;
                gloss: string}

let empty_meaning = {mng_id = (-1);
                     name = "";
                     variant = "";
                     plwnluid = (-1);
                     gloss = ""}

type schema = {sch_id: int; opinion: opinion; reflexiveMark: bool; aspect: aspect;
               negativity: negation; predicativity: pred; positions: position list; text_rep: string}

type lex_entry =
    SimpleLexEntry of string * string
  | LexEntry of int * string * string * restr * position list
  | ComprepNPEntry of string * restr * position list


module OrderedEntry = struct
  type  t = lex_entry
  let compare = compare
end

module EntrySet = Xset.Make(OrderedEntry)

module OrderedAbbr = struct
  type  t = phrase_abbr
  let compare = compare
end

module AbbrMap = Xmap.Make(OrderedAbbr)

module OrderedComp = struct
  type  t = comp_type
  let compare = compare
end

module CompMap = Xmap.Make(OrderedComp)

type example = {exm_id: int;
                meaning: int;
                phrases: (int * int * int) list;
                sentence: string;
                source: string;
                opinion: opinion;
                note: string}

type argument = {arg_id: int;
                 role: string;
                 role_attribute: string;
                 sel_prefs: sel_prefs list}

type frame  = {frm_id: int;
               opinion: opinion;
               meanings: int list;
               arguments: argument list}

type connection = {argument: int;
                   phrases: (int * int list) list}

type alternation = {schema: int; frame: int; connections: connection list}

type entry = {ent_id: int;
              status: string;
              form_orth: string;
              form_pos: string;
              schemata: schema list;
              examples: example list;
              frames: frame list;
              meanings: meaning list;
              alternations: alternation list}

let empty_entry = {ent_id=(-1); status=""; form_orth=""; form_pos=""; schemata=[]; examples=[];
                   frames=[]; meanings=[]; alternations=[]}

type connected = {sch_id: int;
                  frm_id: int;
                  sopinion: opinion;
                  fopinion: opinion;
                  meanings: meaning list;
                  negativity: negation;
                  predicativity: pred;
                  aspect: aspect;
                  schema: position list;
                  examples: (opinion * string) list}
