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
 
open SemTypes  
  
let mrow nl l = Xml.Element("mrow",[],(if nl then [Xml.Element("mspace",["linebreak","newline"],[])] else []) @ l)
  
let variable (v,sub) = 
  if sub = "" then Xml.Element("mi",[],[Xml.PCData v]) else
  Xml.Element("msub",[],[Xml.Element("mi",[],[Xml.PCData v]);Xml.Element("mn",[],[Xml.PCData sub])])
  
let rec mml_concat s = function
    [] -> failwith "mml_concat"
  | [x] -> [x]
  | x :: l -> x :: s :: mml_concat s l
  
let mml_mo s = Xml.Element("mo",[],[Xml.PCData s])
  
let mml_of_quant = function
    TConst(s,[]) -> Xml.Element("mi",[],[Xml.PCData s])
  | _ -> failwith "mml_of_quant"  
  
let rec mml_of_mrl_term = function
  | Variable v -> variable v
  | List l -> failwith "mml_of_mrl_term: ni"
  | Indexical v -> failwith "mml_of_mrl_term: ni"
(*   | Int n -> string_of_int n *)
  | String s -> Xml.Element("mi",[],[Xml.PCData ("„" ^ s ^ "”")])
  | Term("count",[x]) -> mrow false ([mml_mo "|"; mml_of_mrl_term x; mml_mo "|"])
  | Term(c,[]) -> Xml.Element("mi",[],[Xml.PCData ("" ^ c ^ "")])
  | Term(c,l) -> mrow false ([Xml.Element("mi",[],[Xml.PCData c]); mml_mo "("] @
                 mml_concat (mml_mo ",") (Xlist.map l mml_of_mrl_term) @
                 [mml_mo ")"])
  
let rec mml_of_mrl_formula nl p = function
    Conj []  -> mml_of_mrl_formula nl false True
  | Conj [t]  -> mml_of_mrl_formula nl p t
  | Conj l -> mrow false ((if p then [mml_mo "("] else []) @ 
              (mml_concat (mml_mo "∧") (Xlist.map l (mml_of_mrl_formula nl true))) @ 
              (if p then [mml_mo ")"] else []))
  | Disj []  -> mml_of_mrl_formula nl false (Neg True)
  | Disj [t]  -> mml_of_mrl_formula nl p t
  | Disj l -> mrow false ((if p then [mml_mo "("] else []) @ 
              (mml_concat (mml_mo "∨") (Xlist.map l (mml_of_mrl_formula nl true))) @ 
              (if p then [mml_mo ")"] else []))
  | Neg f -> mrow false [mml_mo "⌐"; mml_of_mrl_formula nl true f]
  | Exist(v,t,f) -> failwith "mml_of_mrl_formula: ni"
  | Exist1(v,f) -> 
       mrow nl [mml_mo "∃"; mml_mo "["; variable v; mml_mo ","; mml_of_mrl_formula nl false f; mml_mo "]"]
  | Exist2(v,t,f) -> 
       mrow nl [mml_mo "∃"; mml_mo "["; variable v; mml_mo ","; mml_of_mrl_formula nl false t;
       mml_mo ","; mml_of_mrl_formula nl false f; mml_mo "]"]
  | ForAll(v,t,f) -> 
       mrow nl [mml_mo "∀"; mml_mo "["; variable v; mml_mo ","; mml_of_mrl_formula nl false t;
       mml_mo ","; mml_of_mrl_formula nl false f; mml_mo "]"]
  | Quant1(s,v,f) -> 
       mrow nl [mml_of_quant s; mml_mo "["; variable v; mml_mo ","; mml_of_mrl_formula nl false f; mml_mo "]"]
  | Quant2(s,v,t,f) -> 
       mrow nl [mml_of_quant s; mml_mo "["; variable v; mml_mo ","; mml_of_mrl_formula nl false t;
       mml_mo ","; mml_of_mrl_formula nl false f; mml_mo "]"]
  | Dscr(t,f) -> 
       mrow false [Xml.Element("mi",[],[Xml.PCData "dscr"]); mml_mo "["; mml_of_mrl_term t;
       mml_mo ","; mml_of_mrl_formula nl false f; mml_mo "]"]
  | Pred(c,l) -> mrow false ([Xml.Element("mi",[],[Xml.PCData c]); mml_mo "("] @
                 mml_concat (mml_mo ",") (Xlist.map l mml_of_mrl_term) @
                 [mml_mo ")"])
  | Equal(s,t) -> mrow false [mml_of_mrl_term s; mml_mo "="; mml_of_mrl_term t]
  | Greater(s,t) -> mrow false [mml_of_mrl_term s; mml_mo ">"; mml_of_mrl_term t]
  | True -> Xml.Element("mi",[],[Xml.PCData "true"])
  | Handle h -> failwith "mml_of_mrl_formula: ni"
  | Seam f -> (*"{[}" ^*) mml_of_mrl_formula nl p f (*^ "]"*)
  | Requires(v,f) -> (*String.concat "," (StringSet.to_list v) ^ ":" ^*) mml_of_mrl_formula nl p f
  | Cut f -> (*"|" ^*) mml_of_mrl_formula nl p f
  | Context c -> failwith "mml_of_mrl_formula: ni"
  | Position(_,f) -> (*"|" ^*) mml_of_mrl_formula nl p f

(*let rec mml_of_mrl_rec = function
  _ -> Xml.Element("mrow false",[],[Xml.Element("mi",[],[Xml.PCData "x"]);
                              Xml.Element("mo",[],[Xml.PCData "+"]);
                              Xml.Element("mi",[],[Xml.PCData "y"])])*)
 
let mml_of_mrl mrl = 
  Xml.Element("math",["xmlns","http://www.w3.org/1998/Math/MathML"],[mml_of_mrl_formula true false mrl])
