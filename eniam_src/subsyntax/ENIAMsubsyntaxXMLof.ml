(*
 *  ENIAMsubsyntax: MWE, abbreviation and sentence detecion for Polish
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

open ENIAMsubsyntaxTypes
open Printf

let token_extarray t =
  Xml.Element("tokens",[], List.rev (Int.fold 0 (ExtArray.size t - 1) [] (fun l id ->
    ENIAMtokens.xml_of_token_env id (ExtArray.get t id) :: l)))

let token_list paths msg =
  if msg = "" then Xml.Element("tokens",[],Xlist.map paths (fun t -> ENIAMtokens.xml_of_token_env (-1) t))
  else Xml.Element("error",[],[Xml.PCData msg])

let xml_of_dep_sentence paths = failwith "xml_of_dep_sentence: ni"
(*  List.rev (Int.fold 0 (Array.length paths - 1) [] (fun l conll_id ->
    let id,super,label = paths.(conll_id) in
    Xml.Element("edge",["conll_id",string_of_int conll_id;"id",string_of_int id] @
      (if super = (-1) then [] else ["super",string_of_int super]) @
      (if label = "" then [] else ["label",label]),[]) :: l))*)

let xml_of_edge (id,lnode,rnode) =
    Xml.Element("edge",["id",string_of_int id;"lnode",string_of_int lnode;"rnode",string_of_int rnode],[])

let set_mode m =
  if m = "" then [] else ["mode",m]

let rec sentence m = function
    RawSentence s -> Xml.Element("RawSentence",set_mode m,[Xml.PCData s])
  | StructSentence(paths,last) -> Xml.Element("StructSentence",(set_mode m) @ ["last",string_of_int last],Xlist.map paths xml_of_edge)
  | DepSentence paths -> failwith "ENIAMsubsyntaxXMLof.sentence: ni" (*Xml.Element("DepSentence",
          (set_mode m) @ ["size",string_of_int (Array.length paths)],xml_of_dep_sentence paths)*) (* FIXME *)
  | QuotedSentences sentences ->
      Xml.Element("QuotedSentences",set_mode m,Xlist.map sentences (fun p ->
        Xml.Element("Sentence",["id",p.id;"beg",string_of_int p.beg;"len",string_of_int p.len;"next",string_of_int p.next],[sentence "" p.sentence])))
  | AltSentence l -> Xml.Element("AltSentence",set_mode m,Xlist.map l (fun (m,t) -> sentence (ENIAMsubsyntaxStringOf.mode m) t))
  | ErrorSentence s -> Xml.Element("ErrorSentence",set_mode m,[Xml.PCData s])

let rec paragraph m = function
    RawParagraph s -> Xml.Element("RawParagraph",set_mode m,[Xml.PCData s])
  | StructParagraph sentences ->
      Xml.Element("StructParagraph",set_mode m,Xlist.map sentences (fun p ->
        Xml.Element("Sentence",["id",p.id;"beg",string_of_int p.beg;"len",string_of_int p.len;"next",string_of_int p.next],[sentence "" p.sentence])))
  | AltParagraph l -> Xml.Element("AltParagraph",set_mode m,Xlist.map l (fun (m,t) -> paragraph (ENIAMsubsyntaxStringOf.mode m) t))
  | ErrorParagraph s -> Xml.Element("ErrorParagraph",set_mode m,[Xml.PCData s])

let rec text m = function
    RawText s -> Xml.Element("RawText",set_mode m,[Xml.PCData s])
  | StructText paragraphs -> Xml.Element("StructText",set_mode m,Xlist.map paragraphs (paragraph ""))
  | AltText l -> Xml.Element("AltText",set_mode m,Xlist.map l (fun (m,t) -> text (ENIAMsubsyntaxStringOf.mode m) t))
  | ErrorText s -> Xml.Element("ErrorText",set_mode m,[Xml.PCData s])

let text_and_tokens tex tok msg =
  if msg = "" then Xml.Element("data",[],[text "" tex; token_extarray tok])
  else Xml.Element("error",[],[Xml.PCData msg])
