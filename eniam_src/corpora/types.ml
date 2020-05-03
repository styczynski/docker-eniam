(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
 *  Copyright (C) 2016 Daniel Oklesinski <oklesinski dot daniel atSPAMfree gmail dot com>
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

type conll_token =
  {c_id:int; c_orth:string; c_lemma:string; c_cat:string; c_interp:string list;
   c_super:int; c_label:string; c_beg:int; c_len:int}

type conll_sentence =
  {s_id:string; s_text:string; s_tokens:conll_token list}

type info_sentence =
  {i_id:string; i_text:string; i_tokens:string list}

let resource_path =
  try Sys.getenv "ENIAM_RESOURCE_PATH"
  with Not_found ->
    if Sys.file_exists "/usr/share/eniam" then "/usr/share/eniam" else
    if Sys.file_exists "/usr/local/share/eniam" then "/usr/local/share/eniam" else
    if Sys.file_exists "resources" then "resources" else
    failwith "resource directory does not exists"
