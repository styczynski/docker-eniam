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

open ENIAMwalTypes
open Xstd

let correct_walenty entry =
  if entry.form_orth = "podobać" then
    {entry with schemata=Xlist.map entry.schemata (fun s ->
         {s with positions=Xlist.map s.positions (fun p ->
              if p.gf=SUBJ then {p with morfs=List.flatten (Xlist.map p.morfs (function
                    MorfId 126 -> []
                  | m -> [m]))}
              else p)})}
  else entry

let load_walenty walenty_filename expands_filename =
  print_endline "load_walenty 1";
  let walenty,phrases = ENIAMwalTEI.load_walenty walenty_filename in
  print_endline "load_walenty 2";
  let walenty = Xlist.rev_map walenty correct_walenty in
  print_endline "load_walenty 3";
  let expands = ENIAMwalTEI.load_expands expands_filename in
  print_endline "load_walenty 4";
  let meanings =
    Xlist.fold walenty IntMap.empty (fun meanings entry ->
      Xlist.fold entry.meanings meanings (fun meanings meaning ->
        IntMap.add meanings meaning.mng_id meaning)) in
  print_endline "load_walenty 5";
  let connected_walenty =
    Xlist.fold walenty Entries.empty (fun connected_walenty e ->
        let entries = ENIAMwalConnect.connect e in
        Entries.add_inc_list connected_walenty e.form_pos e.form_orth entries) in
  print_endline "load_walenty 6";
  let schemata_walenty =
    Xlist.fold walenty Entries.empty (fun schemata_walenty e ->
        let entries = ENIAMwalConnect.schemata e in
        Entries.add_inc_list schemata_walenty e.form_pos e.form_orth entries) in
  print_endline "load_walenty 7";
  let expands,compreps,subtypes,equivs,adv_types =
    ENIAMwalRealizations.load_realizations (expands,ENIAMwalTEI.subtypes,ENIAMwalTEI.equivs) in
  print_endline "load_walenty 8";
  let phrases =
    IntMap.map phrases (fun morf ->
        let morf = ENIAMwalRealizations.expand_schema_morf expands morf in
        let morfs = ENIAMwalRealizations.expand_subtypes_morf subtypes morf in
        let morf = List.flatten (Xlist.map morfs (ENIAMwalRealizations.expand_equivs_morf equivs)) in
        morf) in
  print_endline "load_walenty 9";
  let compreps = Xlist.map compreps (fun (lemma,morfs) ->
      lemma, ENIAMwalLex.expand_lexicalizations_morfs morfs) in
  print_endline "load_walenty 10";
  let entries = ENIAMwalLex.extract_lex_entries_comprepnp [] compreps in
  print_endline "load_walenty 11";
  let phrases,entries =
    IntMap.fold phrases (IntMap.empty,entries) (fun (phrases,entries) id morfs ->
        let morfs = ENIAMwalLex.expand_lexicalizations_morfs morfs in
        let morfs,entries = Xlist.fold morfs ([],entries) ENIAMwalLex.extract_lex_entries in
        IntMap.add phrases id morfs, entries) in
  print_endline "load_walenty 12";
  let entries = Xlist.fold entries Entries.empty (fun entries (pos,lemma,entry) ->
      Entries.add_inc entries pos lemma entry) in
  print_endline "load_walenty 13";
  let entries = Entries.map2 entries (fun pos lemma entries -> EntrySet.to_list (EntrySet.of_list entries)) in
  print_endline "load_walenty 14";
  let entries = Entries.flatten_map entries (fun pos lemma entry ->
      ENIAMwalLex.expand_restr [] lemma pos entry) in
    (* let entries =
      StringMap.mapi entries (fun pos entries2 ->
        StringMap.mapi entries2 (fun lemma entries3 ->
            EntrySet.fold entries3 [] (fun entries3 entry ->
                (ENIAMwalLex.expand_restr [] lemma pos entry) @ entries3))) in *)
  print_endline "load_walenty 15";
  connected_walenty, schemata_walenty, phrases, entries, meanings, adv_types

let print_entries filename entries =
  File.file_out filename (fun file ->
      Entries.iter entries (fun pos lemma entry ->
          Printf.fprintf file "%s\t%s\t%s\n" pos lemma (ENIAMwalStringOf.lex_entry entry)))

let print_phrases filename phrases =
  File.file_out filename (fun file ->
      IntMap.iter phrases (fun id morfs ->
          let morfs = Xlist.map morfs ENIAMwalStringOf.morf in
          Printf.fprintf file "%d\t%s\n" id (String.concat "\t" morfs)))

let print_schemata filename schemata =
  File.file_out filename (fun file ->
      Entries.iter schemata (fun pos lemma (opinion,(n,p,a),schema) ->
          Printf.fprintf file "%s\t%s\t%s\t%s\t%s\t%s\t%s\n" pos lemma
            (ENIAMwalStringOf.opinion opinion)
            (ENIAMwalStringOf.negation n)
            (ENIAMwalStringOf.pred p)
            (ENIAMwalStringOf.aspect a)
            (ENIAMwalStringOf.simple_schema schema)))

let print_connected filename connected =
  File.file_out filename (fun file ->
      Entries.iter connected (fun pos lemma c(*sopinion,fopinion,meanings,(n,p,a),schema*) ->
          Printf.fprintf file "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n" pos lemma
            (ENIAMwalStringOf.opinion c.sopinion)
            (ENIAMwalStringOf.opinion c.fopinion)
            (String.concat "," (Xlist.map c.meanings (fun m -> string_of_int m.mng_id)))
            (ENIAMwalStringOf.negation c.negativity)
            (ENIAMwalStringOf.pred c.predicativity)
            (ENIAMwalStringOf.aspect c.aspect)
            (ENIAMwalStringOf.connected_schema c.schema)))

let split_tokens s =
  let l = List.flatten (Xlist.map (Str.full_split (Str.regexp " \\|,\\|-") s) (function
        Str.Delim " " -> []
      | Str.Delim s -> [s]
      | Str.Text s -> [s])) in
  String.concat " " l

let print_fixed filename fixed =
  File.file_out filename (fun file ->
      StringSet.iter fixed (fun s ->
          let t = split_tokens s in
          Printf.fprintf file "%s\t%s\tfixed\n" t s))

let print_adv_types filename adv_types =
  File.file_out filename (fun file ->
      Xlist.iter adv_types (fun (m,l) ->
          Xlist.iter l (fun s ->
              Printf.fprintf file "%s\t%s\n" s m)))

let add_fixed fixed = function
    Phrase (FixedP s) -> StringSet.add fixed s
  | SimpleLexArg(s,FIXED) -> StringSet.add fixed s
  | LexArg(_,s,FIXED) -> StringSet.add fixed s
  | _ -> fixed

let find_fixed_schema fixed schema =
  Xlist.fold schema fixed (fun schema p ->
      Xlist.fold p.morfs fixed add_fixed)

let find_fixed phrases entries =
  let fixed = IntMap.fold phrases StringSet.empty (fun fixed _ morfs ->
      Xlist.fold morfs fixed add_fixed) in
  Entries.fold entries fixed (fun fixed pos lemma -> function
        SimpleLexEntry(s,"fixed") -> StringSet.add fixed s
      | SimpleLexEntry(s,_) -> fixed
      | LexEntry(_,s,"fixed",_,schema) -> find_fixed_schema (StringSet.add fixed s) schema
      | LexEntry(_,_,_,_,schema) -> find_fixed_schema fixed schema
      | ComprepNPEntry(_,_,schema) -> find_fixed_schema fixed schema)

let print_meanings filename meanings =
  File.file_out filename (fun file ->
      IntMap.iter meanings (fun _ m ->
          Printf.fprintf file "%d\t%s\t%s\t%d\t%s\n" m.mng_id m.name m.variant m.plwnluid m.gloss))

(* let connected_walenty, schemata_walenty, phrases, entries, meanings = load_walenty
    "/home/yacheu/Dokumenty/NLP resources/Walenty/walenty_20170311.xml"
    "/home/yacheu/Dokumenty/NLP resources/Walenty/phrase_types_expand_20170311.xml" *)

(* Generowanie zasobów *)
let _ =
  if Array.length Sys.argv < 3 then print_endline "missing argument" else (
    let connected_walenty, schemata_walenty, phrases, entries, meanings, adv_types = load_walenty Sys.argv.(1) Sys.argv.(2) in
    print_entries "resources/entries.tab" entries;
    print_phrases "resources/phrases.tab" phrases;
    print_schemata "resources/schemata.tab" schemata_walenty;
    print_connected "resources/connected.tab" connected_walenty;
    print_fixed "resources/fixed.tab" (find_fixed phrases entries);
    print_meanings "resources/meanings.tab" meanings;
    print_adv_types "resources/adv_modes.tab" adv_types;
    ())
