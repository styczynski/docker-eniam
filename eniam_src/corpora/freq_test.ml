(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
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

open Xstd

let _ =
  let l = File.load_tab "../resources/NKJP1M/NKJP1M-frequency.tab" (function
      [orth; lemma; interp; freq] -> orth, lemma, interp, int_of_string freq
    | l -> failwith ("load_frequencies: " ^ String.concat "\t" l)) in
  let qmap = Xlist.fold l StringQMap.empty (fun qmap (orth, lemma, interp, freq) ->
    let interp = List.hd (Xstring.split ":" interp) in
    StringQMap.add_val qmap (lemma ^ "\t" ^ interp) freq) in
  StringQMap.iter qmap (fun k v -> Printf.printf "%d\t%s\n" v k)
