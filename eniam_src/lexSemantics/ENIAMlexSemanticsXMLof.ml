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

open ENIAMlexSemanticsTypes
open Printf

let lex_sems t =
  Xml.Element("lex_sems",[], [](*List.rev (Int.fold 0 (ExtArray.size t - 1) [] (fun l id ->
    ENIAMtokens.xml_of_token_env id (ExtArray.get t id) :: l))*))

let text_and_tokens_and_lex_sems tex tok lex msg =
  if msg = "" then Xml.Element("data",[],[ENIAMsubsyntaxXMLof.text "" tex; ENIAMsubsyntaxXMLof.token_extarray tok;lex_sems lex])
  else Xml.Element("error",[],[Xml.PCData msg])
