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
    CVar(v,w) -> Xml.Element("cvar",["name",v;"num",w],[])
  | CDef v -> Xml.Element("cdef",["name",v],[])
  | COr l -> Xml.Element("cor",[], Xlist.map l context_term)
  | CEmpty -> Xml.Element("cempty",[],[])
  | CModel m -> Xml.Element("cmodel",[],[Xml.PCData (Big_int.string_of_big_int m)])

let qcons_attrs i p r =
  if !assign_semform_data_flag then
    ["id",i;"left",string_of_int p;"right",string_of_int r]
  else ["id",i]

let rec lfg_term = function
    Cons(ids,s) -> Xml.Element("cons",[],[Xml.Element("value",[],[Xml.PCData s])])
  | QCons(ids,s,i,[],[],p,r) -> Xml.Element("qcons",qcons_attrs i p r,[Xml.Element("value",[],[Xml.PCData s])])
  | QCons(ids,s,i,l,[],p,r) -> Xml.Element("qcons",qcons_attrs i p r,[Xml.Element("value",[],[Xml.PCData s]);
                                      Xml.Element("args",[],Xlist.map l lfg_term)])
  | QCons(ids,s,i,l,l2,p,r) -> Xml.Element("qcons",qcons_attrs i p r,[Xml.Element("value",[],[Xml.PCData s]);
                                      Xml.Element("args",[],Xlist.map l lfg_term);Xml.Element("nosemargs",[],Xlist.map l2 lfg_term)])
  | LVar s -> Xml.Element("lvar",["val",s],[])
  | Compound(ids,l) -> Xml.Element("compound",[],Xlist.map l (fun (e,v) -> Xml.Element("attr",["label",e],[lfg_term v])))
  | Set(ids,l) -> Xml.Element("set",[],Xlist.map l lfg_term)
  | Coordination(ids,l,l2) -> Xml.Element("coordination",[],Xlist.map l lfg_term @ Xlist.map l2 (fun (e,v) -> Xml.Element("attr",["label",e],[lfg_term v])))
  | Loop(ids,path) -> Xml.Element("loop",[],Xlist.map path (fun s -> Xml.Element("label",["val",s],[])))
  | Context l -> Xml.Element("context",[],Xlist.map l (fun (c,t) -> Xml.Element("variant",[],[context_term c;lfg_term t])))

let sentence s =
  Xml.Element("sentence",[],[Xml.PCData s])

let choices c =
  Xml.Element("choices",[],Xlist.map c (fun (l,c) ->
    Xml.Element("choice",[],[Xml.Element("list",[],Xlist.map l context_term);context_term c])))

let fstructure fs =
  Xml.Element("tree",[],[lfg_term fs])

let form fm =
  Xml.Element("form",[], Xlist.map fm (fun (c,(id,label,left,right)) ->
    Xml.Element("node",["id",id;"label",label;"left",string_of_int left;"right",string_of_int right],[context_term c])))

let print filename s c fs fm =
  let xml = Xml.Element("fstructure",[],[sentence s;choices c;fstructure fs;form fm]) in
  File.file_out filename (fun file ->
    Printf.fprintf file "%s" (Xml.to_string xml))
