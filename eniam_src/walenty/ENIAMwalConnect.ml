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
open ENIAMwalTypes

let process_morfs morfs =
  Xlist.fold morfs IntMap.empty (fun morfs -> function
        MorfId id -> IntMap.add morfs id (MorfId id)
      | _ -> failwith "process_morfs")

let process_positions positions =
  Xlist.fold positions IntMap.empty (fun positions position ->
      IntMap.add positions position.psn_id position)

let process_schemata schemata =
  Xlist.fold schemata IntMap.empty (fun schemata (schema : schema)  ->
    let atrs = schema.negativity, schema.predicativity, schema.aspect in
    let positions = process_positions schema.positions in
    IntMap.add schemata schema.sch_id (schema.reflexiveMark,schema.opinion,atrs,positions))

let process_arguments arguments =
  Xlist.fold arguments IntMap.empty (fun arguments argument ->
    IntMap.add arguments argument.arg_id argument)

let process_frames frames =
  Xlist.fold frames IntMap.empty (fun frames frame ->
      let arguments = process_arguments frame.arguments in
      IntMap.add frames frame.frm_id (frame,arguments))

let process_meanings meanings =
  Xlist.fold meanings IntMap.empty (fun meanings meaning ->
      IntMap.add meanings meaning.mng_id meaning(*meaning.name ^ " " ^ meaning.variant*))

let process_sel_pref arguments = function
    SynsetId s -> SynsetId s(*try ENIAMplWordnet.synset_name s with Not_found -> "unknown"*)
  | Predef s -> Predef s
  | RelationArgId(s,id) ->
    let arg = try IntMap.find arguments id with Not_found -> failwith "process_sel_pref" in
    RelationRole(s,arg.role,arg.role_attribute)
  | RelationRole _ -> failwith "process_sel_pref"

let connect entry =
  let schemata = process_schemata entry.schemata in
  let frames = process_frames entry.frames in
  let meanings = process_meanings entry.meanings in
  Xlist.fold entry.alternations [] (fun found alt ->
    let refl,opinion,(n,p,a),positions = IntMap.find schemata alt.schema in
    let frame,arguments = IntMap.find frames alt.frame in
    let conn_positions = if refl then [ENIAMwalTEI.refl_position] else [] in
    let conn_positions = Xlist.fold alt.connections conn_positions (fun conn_positions conn ->
      let arg = IntMap.find arguments conn.argument in
      let sel_prefs = Xlist.map arg.sel_prefs (process_sel_pref arguments) in
      Xlist.fold conn.phrases conn_positions (fun conn_positions (position_id,phrase_ids) ->
          let position = IntMap.find positions position_id in
          let phrases = process_morfs position.morfs in
          let morfs = Xlist.fold phrase_ids [] (fun morfs phrase_id ->
              try IntMap.find phrases phrase_id :: morfs
              with Not_found -> if entry.form_orth <> "podobać" then Printf.printf "connect: %s\n%!" entry.form_orth;morfs) in
          {position with role=arg.role; role_attr=arg.role_attribute; sel_prefs=sel_prefs;
                         morfs=List.rev morfs} :: conn_positions)) in
    let meanings = List.rev (Xlist.rev_map frame.meanings (fun id ->
          try IntMap.find meanings id with Not_found -> {empty_meaning with mng_id=id})) in
    {sch_id=alt.schema; frm_id=alt.frame; sopinion=opinion; fopinion=frame.opinion; meanings=meanings;
      negativity=n; predicativity=p;aspect=a; schema=conn_positions; examples=[]} :: found)

let schemata entry =
  let schemata = process_schemata entry.schemata in
  IntMap.fold schemata [] (fun found _ (refl,opinion,schema_atrs,positions) ->
      let positions = IntMap.fold positions [] (fun positions _ position -> position :: positions) in
      let positions = if refl then ENIAMwalTEI.refl_position :: positions else positions in
      (opinion,schema_atrs,positions) :: found)
