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

let rec context_term = function
    Xml.Element("cvar",["name",v;"num",w],[]) ->  CVar(v,w)
  | Xml.Element("cdef",["name",v],[]) -> CDef v
  | Xml.Element("cor",[],l) -> COr(Xlist.map l context_term)
  | Xml.Element("cempty",[],[]) -> CEmpty
  | Xml.Element("cmodel",[],[Xml.PCData s]) -> CModel (Big_int.big_int_of_string s)
  | _ -> failwith "context_term"

let rec split_coordination rev = function
    Xml.Element("attr",_,_) :: _ as l -> List.rev rev, l
  | x :: l -> split_coordination (x :: rev) l
  | [] -> List.rev rev, []

let rec lfg_term = function
    Xml.Element("cons",[],[Xml.Element("value",[],[Xml.PCData s])]) -> Cons(StringMap.empty,s)
  | Xml.Element("qcons",["id",i;"left",p;"right",r],[Xml.Element("value",[],[Xml.PCData s])]) ->
      QCons(StringMap.empty,s,i,[],[],int_of_string p,int_of_string r)
  | Xml.Element("qcons",["id",i;"left",p;"right",r],[Xml.Element("value",[],[Xml.PCData s]);
                Xml.Element("args",[],l)]) -> QCons(StringMap.empty,s,i,Xlist.map l lfg_term,[],int_of_string p,int_of_string r)
  | Xml.Element("qcons",["id",i;"left",p;"right",r],[Xml.Element("value",[],[Xml.PCData s]);
                Xml.Element("args",[],l);Xml.Element("nosemargs",[],l2)]) ->
                  QCons(StringMap.empty,s,i,Xlist.map l lfg_term,Xlist.map l2 lfg_term,int_of_string p,int_of_string r)
  | Xml.Element("lvar",["val",s],[]) -> LVar s
  | Xml.Element("compound",[],l) -> Compound(StringMap.empty,Xlist.map l (function
        Xml.Element("attr",["label",e],[v]) -> e,lfg_term v
      | _ -> failwith "lfg_term 1"))
  | Xml.Element("set",[],l) -> Set(StringMap.empty,Xlist.map l lfg_term)
  | Xml.Element("coordination",[],l) ->
      let l,l2 = split_coordination [] l in
      Coordination(StringMap.empty,Xlist.map l lfg_term,Xlist.map l2 (function
        Xml.Element("attr",["label",e],[v]) -> e,lfg_term v
      | _ -> failwith "lfg_term 2"))
  | Xml.Element("loop",[],path) -> Loop(StringMap.empty,Xlist.map path (function
        Xml.Element("label",["val",s],[]) -> s
      | _ -> failwith "lfg_term 3"))
  | Xml.Element("context",[],l) -> Context(Xlist.map l (function
        Xml.Element("variant",[],[c;t]) -> context_term c,lfg_term t
      | _ -> failwith "lfg_term 4"))
  | _ -> failwith "lfg_term 5"

let choice = function
    Xml.Element("choice",[],[Xml.Element("list",[],l);c]) -> Xlist.map l context_term, context_term c
  | _ -> failwith "choice"

let fstructure = function
    Xml.Element("fstructure",[],[
      Xml.Element("sentence",[],[Xml.PCData s]);
      Xml.Element("choices",[],choices);
      Xml.Element("tree",[],[tree])]) -> s, Xlist.map choices choice, lfg_term tree
  | _ -> failwith "fstructure"
