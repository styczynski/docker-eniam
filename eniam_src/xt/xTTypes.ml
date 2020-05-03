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

open Xstd

type context_term =
    CVar of string * string
  | CDef of string
  | COr of context_term list
  | CEmpty
  | CModel of Big_int.big_int

type lfg_term =
    Cons of context_term StringMap.t * string
  | QCons of context_term StringMap.t * string * string * lfg_term list * lfg_term list * int * int
  | LVar of string
  | Compound of context_term StringMap.t * (string * lfg_term) list
  | Coordination of context_term StringMap.t * lfg_term list * (string * lfg_term) list
  | Set of context_term StringMap.t * lfg_term list
  | Loop of context_term StringMap.t * string list
  | Context of (context_term * lfg_term) list

type prolog_graph = {p_sentence: string; p_id: string; p_choices: (context_term list * context_term) list; p_defines: context_term StringMap.t;
               p_in_sets: (context_term * string) list StringMap.t;
               p_equi: (context_term * string) list StringMap.t;
               p_constraints: (context_term * lfg_term) list StringMap.t;
               p_subfields: (context_term * (string * lfg_term)) list StringMap.t;
               p_subsumes: (context_term * (string * lfg_term)) list;
               p_subtree: (context_term * (string * string * string * string)) list;
               p_phi: (context_term * (string * string)) list;
               p_terminal: (context_term * (string * string * string list)) list;
               p_semform_data: (context_term * (string * int * int)) list StringMap.t;
               p_fspan: (context_term * (string * string * string)) list;
               p_surfaceform: (context_term * (string * string * int * int)) list}

let adjuncts = ref ["ADJUNCT";"XADJUNCT"]
(*let assign_punctuation_flag = ref false
let assign_prep_cases_flag = ref false
let assign_semform_data_flag = ref false*)

let assign_punctuation_flag = ref true
let assign_prep_cases_flag = ref true
let assign_semform_data_flag = ref true
let disambiguate_flag = ref true

let max_count_paths = ref 10000
