(*
 *  ENIAM: Categorial Syntactic-Semantic Parser for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Xstd

type mrl_variable = string * string
(**type linear_variable = string**)
type const = string

type context_term =
    CVar of string * string
  | COr of context_term list
  | CNeg of context_term
  | CEmpty

type type_arg =
    TArg of string
  | TWith of type_arg list

and type_term =  
    TConst of string * type_arg list
  | TMod of type_term * type_term 
  | TName of string
  | TVariant of type_term * type_term
  
type mrl_formula =
    Conj of mrl_formula list
  | Disj of mrl_formula list
(*  | Impl of mrl_formula * mrl_formula*)
  | Neg of mrl_formula
  | Exist of mrl_variable * mrl_term * mrl_formula
  | Exist1 of mrl_variable * mrl_formula
  | Exist2 of mrl_variable * mrl_formula * mrl_formula
  | ForAll of mrl_variable * mrl_formula * mrl_formula
  | Quant1 of type_term * mrl_variable * mrl_formula
  | Quant2 of type_term * mrl_variable * mrl_formula * mrl_formula
  | Dscr of mrl_term * mrl_formula
  | Pred of const * mrl_term list
  | Equal of mrl_term * mrl_term
  | Greater of mrl_term * mrl_term
  | True
  | Handle of string
  | Seam of mrl_formula
  | Requires of StringSet.t * mrl_formula
  | Cut of mrl_formula
  | Context of context_term
  | Position of int * mrl_formula

and mrl_term =
    Variable of mrl_variable
  | List of mrl_term list
  | Indexical of string
  | String of string
  | Term of const * mrl_term list

type mrs_formula = 
    MRSeps of string * mrl_formula
  | MRSqeq of string * string
  | MRStop of string
  | MRSpos of string * int
  
