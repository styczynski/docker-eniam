(*
 *  ENIAM_NKJP, an interface for National Corpus of Polish (NKJP).
 *  Copyright (C) 2017 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2017 Institute of Computer Science Polish Academy of Sciences
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

let xml_space = Xml.PCData " "
let xml_err_space = Xml.Element("sp",[],[])

let make_xml_token real_orth orth =
  if real_orth = orth then Xml.PCData orth else
  Xml.Element("err",["cor",orth],[Xml.PCData real_orth])

let rec merge_pcdata = function
    Xml.PCData a :: Xml.PCData b :: l -> merge_pcdata (Xml.PCData(a ^ b) :: l)
  | x :: l -> x :: (merge_pcdata l)
  | [] -> []

let generate_error_sentences sentences =
  let sentences,_,_ = Xlist.fold sentences ([],"","") (fun (sentences,prev_orth,prev_cat) (id_s,tokens,named_tokens) ->
    let no_tokens = Xlist.size tokens in
    let tokens,prev_orth,prev_cat = Xlist.fold tokens ([],prev_orth,prev_cat) (fun (tokens,prev_orth,prev_cat) (_,_,no_spaces,real_orth,orth,_,cat,_) ->
      let tokens = Int.fold 1 no_spaces tokens (fun tokens _ -> xml_space :: tokens) in
      let tokens = if no_spaces = 0 && ValidateTokenizer.is_space_required prev_orth prev_cat orth cat then xml_err_space :: tokens else tokens in
      (make_xml_token real_orth orth) :: tokens, orth, cat) in
    Xml.Element("s",["id",id_s;"length",string_of_int no_tokens],merge_pcdata (List.rev tokens)) :: sentences,prev_orth,prev_cat) in
  Xml.Element("p",[],List.rev sentences)

let generate_error_corpus path out_path =
  ENIAM_NKJP.fold ENIAM_NKJP.nkjp_path () (fun () (name,typ,channel,entries) ->
    (* print_endline name; *)
    let entries = List.rev (Xlist.rev_map entries (fun (id_div,has_ne,paragraphs) ->
      let paragraphs = List.rev (Xlist.rev_map paragraphs (fun (paragraph,sentences) ->
        generate_error_sentences sentences)) in
      Xml.Element("div",["id",string_of_int id_div],paragraphs))) in
    let xml = Xml.Element("source",["id",name;"type",typ;"channel",channel],entries) in
    File.file_out (out_path ^ name ^ ".xml") (fun file ->
      output_string file (Xml.to_string_fmt xml)))

let _ = generate_error_corpus ENIAM_NKJP.nkjp_path "NKJP1M_spelling_errors/"
