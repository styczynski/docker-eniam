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

let convert_filename filename =
  let n = String.length filename in
  if n < 3 then filename ^ ".xml" else
  if String.sub filename (n-3) 3 = ".pl" then
    String.sub filename 0 (n-3) ^ ".xml"
  else filename ^ ".xml"

let filename = ref ""

let filename_fun s =
  filename := s

let spec_list = [
  "-a", Arg.Set assign_punctuation_flag, "add attribute PUN with punctuation to f-structure";
  "-b", Arg.Clear assign_punctuation_flag, "do not add attribute PUN with punctuation to f-structure (default)";
  "-c", Arg.Set assign_prep_cases_flag, "add attribute PCASE attributes with preposition cases to f-structure";
  "-d", Arg.Clear assign_prep_cases_flag, "do not add attribute PCASE attributes with preposition cases to f-structure (default)";
  "-e", Arg.Set assign_semform_data_flag, "add semform data (lexeme input positions) to PRED attributes";
  "-f", Arg.Clear assign_semform_data_flag, "do add semform data (lexeme input positions) to PRED attributes (default)";
  "-s", Arg.String (fun s -> adjuncts := s :: (!adjuncts)), "add attribute with set value (by default attributes ADJUNCT and XADJUNCT have set values)";
  "-t", Arg.String (fun s -> adjuncts := List.filter (fun x -> x <> s) (!adjuncts)), "remove attribute with set value (by default attributes ADJUNCT and XADJUNCT have set values)";
  ]

let usage_msg = "Syntax: test [OPTIONS] <filename> FILE\nConverts prolog file with XLE f-structure into ENIAM format.\n\nOptional arguments:"

let _ =
  Arg.parse spec_list filename_fun usage_msg;
  if !filename <> "" then
    let sentence,choices,fstructure,form = XTNormalizer.load_fstructure (!filename) in
    XTXmlOf.print (convert_filename (!filename)) sentence choices fstructure form else
  Arg.usage spec_list usage_msg
