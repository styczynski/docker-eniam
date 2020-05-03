(*
 *  ENIAMmorphology, a morphological analyser and a guesser for Polish
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

type form = {orth: string; interp: string; freq: int; genre: string; validated: bool}
type entry = {lemma: string; cat: string; forms: form list; proper_type: string; ndm: bool; stem: string}

type star = Productive | Star | Ndm

type rule = {star: star; pref: string; find: string; set: string; tags: (string * string) list;
  interp: string; id: string; freq: int}

let resource_path =
  try Sys.getenv "ENIAM_RESOURCE_PATH"
  with Not_found ->
    if Sys.file_exists "/usr/share/eniam" then "/usr/share/eniam" else
    if Sys.file_exists "/usr/local/share/eniam" then "/usr/local/share/eniam" else
    if Sys.file_exists "resources" then "resources" else
    failwith "resource directory does not exists"

let alt_filename = resource_path ^ "/morphology/alt.tab"
let stem_filename = resource_path ^ "/morphology/stem.tab"
let rules_filename = resource_path ^ "/morphology/freq_rules.tab"

let alt_supplement_filename = resource_path ^ "/morphology/alt_supplement.tab"
