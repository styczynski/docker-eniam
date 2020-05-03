(*
 *  ENIAMlexSemantics is a library that assigns tokens with lexicosemantic information.
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

open ENIAMtokenizerTypes
open Xstd

type frame = {
  selectors: (ENIAM_LCGlexiconTypes.selector * ENIAM_LCGlexiconTypes.selector_relation * string list) list;
  senses: ((*ENIAMwalTypes.sense **) string * (string * int) list * float) list;
  cats: (string * string list) list;
  positions: ENIAMwalTypes.position list;
  arole: string;
  arole_attr: string;
  arev: bool;
  agf: string;
  sem_args: string list;
  rev_hipero: bool;
  (*snode: string list;*)
  sopinion: ENIAMwalTypes.opinion;
  fopinion: ENIAMwalTypes.opinion;
  }

let empty_frame = {selectors=[]; senses=[]; cats=["X",["X"]]; positions=[]; arole=""; arole_attr=""; arev=false; agf=""; sem_args=[]; rev_hipero=false; (*snode=[];*)
  sopinion=ENIAMwalTypes.Nieokreslony; fopinion=ENIAMwalTypes.Nieokreslony}

type lex_sem = {
  schemata: ((ENIAM_LCGlexiconTypes.selector * ENIAM_LCGlexiconTypes.selector_relation * string list) list *
             (string * string list) list * (* sensy *)
             (*string list **) (* has_context *)
             (ENIAM_LCGtypes.direction * ENIAM_LCGtypes.grammar_symbol) list * (* local_schema *)
             (ENIAM_LCGtypes.direction * ENIAM_LCGtypes.grammar_symbol) list * (* schema *)
             (ENIAM_LCGtypes.direction * ENIAM_LCGtypes.grammar_symbol) list) list; (* distant_schema *)
  lex_entries: ((ENIAM_LCGlexiconTypes.selector * ENIAM_LCGlexiconTypes.selector_relation * string list) list *
                ENIAM_LCGtypes.grammar_symbol) list;
  frames: frame list;
  (* cats: (string * string list) list; *)
  }

let empty_lex_sem = {
  schemata=[]; lex_entries=[]; frames=[]}

let hipero_threshold = 3
let unknown_sense_weight = -1.

let lu_filename = resource_path ^ "/plWordnet/lu.tab"
let ex_hipo_filename = resource_path ^ "/plWordnet/ex_hipo.tab"
let syn_filename = resource_path ^ "/plWordnet/syn.tab"

let predef_filename = resource_path ^ "/lexSemantics/predef_prefs.tab"
let proper_classes_filename = resource_path ^ "/lexSemantics/proper_classes.tab"

let coercions_filename = ENIAMwalTypes.data_path ^ "/coercions.tab"
let proper_cats_filename = ENIAMwalTypes.data_path ^ "/proper_cats.tab"
