(*
 *  ENIAMplWordnet, a converter for Polish Wordnet "Słowosieć".
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

type lu = {lu_id: int; lu_name: string; lu_pos: string; lu_tagcount: string; lu_domain: string; lu_desc: string;
          lu_workstate: string; lu_source: string; lu_variant: string; lu_syn: int}

type syn = {syn_workstate: string; syn_split: string; syn_owner: string; syn_definition: string;
          syn_desc: string; syn_abstract: bool; syn_units: (int * lu) list; syn_pos: string; syn_no_hipo: int; syn_domain: string}

type rels = {r_parent: int; r_child: int; r_relation: int; r_valid: string; r_owner: string}

type rt = {rt_type: string; rt_reverse: int; rt_name: string; rt_description: string;
          rt_posstr: string; rt_display: string; rt_shortcut: string; rt_autoreverse: string; rt_pwn: string; rt_tests: (string * string) list}

let empty_lu = {lu_id=(-1); lu_name=""; lu_pos=""; lu_tagcount=""; lu_domain=""; lu_desc="";
          lu_workstate=""; lu_source=""; lu_variant=""; lu_syn=(-1)}

(* Poniższe dane są uzyskane za pomocą procedury ENIAMplWordnet.count_pwn_relation *)
let pl_pl_relations = IntSet.of_list [
  10; 11; 13; 19; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 34; 35; 36; 37; 38; 39;
  40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 55; 56; 57; 58; 59; 60;
  62; 63; 64; 65; 74; 75; 89; 90; 92; 93; 101; 104; 106; 107; 108; 110; 111; 113; 114; 116;
  117; 118; 119; 120; 121; 122; 124; 125; 126; 127; 129; 130; 131; 134; 136; 137; 138; 140; 141; 142;
  145; 146; 147; 148; 149; 151; 152; 154; 155; 156; 157; 158; 160; 161; 163; 164; 165; 166; 168; 169;
  230; 242; 244
  ]

let en_en_relations = IntSet.of_list [
  170; 171; 172; 173; 174; 175; 176; 177; 178; 179; 180; 181; 182; 183; 184; 185; 186; 187; 188; 189;
  190; 191; 192; 193; 194; 195
  ]

let pl_en_relations = IntSet.of_list [
  201; 202; 203; 205; 206; 207; 208; 210; 212; 222; 228; 235; 238; 239; 3000; 3002; 3005
  ]

let en_pl_relations = IntSet.of_list [
  209; 211; 213; 214; 215; 216; 217; 218; 219; 223; 229; 3006
  ]

(* relacje wyrażone słowotwórczo *)
let morf_relations = IntSet.of_list [
  34; 35; 36; 37; 38; 39; 40; 41; 42; 43; 44; 45; 46; 47; 48; 49; 50; 51; 52; 53; 55; 56; 57; 58; 59; 62; 63; 74; 75; 89; 110; 111; 124;
  131; 134; 136; 141; 142; 148; 149; 151; 152; 154; 155; 156; 157; 158; 160; 161; 163; 164; 165; 166; 168; 169; 242; 244]

(* relacje wyrażone częściowo słowotwórczo *)
let semimorf_relations = IntSet.of_list [
  19; 64; 65; 90; 93; 101; 113; 118; 119; 120; 121; 122; 125; 126; 127; 129; 130; 140; 147]

(* relacje nie wyrażone słowotwórczo *)
let nomorf_relations = IntSet.of_list [
  10; 11; 13; 20; 21; 22; 23; 24; 25; 26; 27; 28; 29; 60; 92; 104; 106; 107; 108; 114; 116; 117; 137; 138; 145; 146; 230]

(* instancja pojęcia *)
let instance_relations = IntSet.of_list [64;65; 106;107; 145]

(* uogólniona synonimia *)
let synonymy_relations = IntSet.of_list [
  (*19?;*) 53; 55; 56; 57; 60; 62; 74; 75; (*108?;*) 110; 111; 129; 130; 131; 134; 136; 140; 141; 142; 147; 151; 152; 168; 244]
(* 51,52;53; 55; 56; 57; 60; 62,141; 63,142; 108 *)

(* stan, cecha, rola *)
let attr_relations = IntSet.of_list [
  51; 52; 63; 89; 92; 118; 124; 126; 146; 148; 149; 154; 155; 156; 157; 158; 160; 161; 163; 164; 165; 166; 169; 242]

(*X ma Y*)
let has_relations = IntSet.of_list [13;
25;20; 26;21; 27;22; 28;23; 29;24;  (* część,całość;  *)
41;34; 42;35; 43;36; 44;37; 45;38; 46;39; 47;40; (* uczestnik,zdarzenie *)
49;48;50; (* uczestnik,zjawisko *)
58;59(*?*);90;93;
116;113;
114;117;
119;]

(*
34 - agens
48 - agens
49 - miejsce
50 - wytwór|rezultat
58 - mieszkaniec
*)

(* antonimia *)
let antonymy_relations = IntSet.of_list [101; 104]

(* czasowniki: kauzacja, presupozycja *)
let caus_relations = IntSet.of_list [120; 121; 122; 125; 127; 137; 138]

let ex_hipo_rels = [11;65;107] @ IntSet.to_list synonymy_relations
let ex_hipero_rels = [10;64;106;145] @ IntSet.to_list synonymy_relations

(* Child (wartość) jest bardziej ogólny niż Parent (klucz) *)

type dir = Parent | Child
type dir2 = Straight | Reverse

let hipo_relations = [
  0,Straight,[10];
  0,Reverse,[11];
  1,Straight,[64;106;145] @ IntSet.to_list synonymy_relations;
  1,Reverse,[65;107] @ IntSet.to_list synonymy_relations;
  4,Straight,[19;108];
  4,Reverse,[19;108];
  3,Straight,[20;21;22;23;24];
  3,Reverse,[25;26;27;28;29];
  ]

let hipo_extensions = [
  1,"cecha","1",Parent,[52; 146; 148; 149; 154; 155; 156; 157; 158; 160; 161; 163; 164; 165; 166; 169; 242];
  1,"cecha","1",Child,[51; 63; 89; 92; 118; 124; 126];
  ]

let hipo_extensions2 = [
  4,"cecha","1",["przymiotnik"; "przysłówek"];
  ]

(*
  holo = "20" (* część -> całość *) (* Holonimy (części) *)
  holo3 = "22" (* część -> całość *) (* Holonimy (miejsca) *)
  holo2 = "23" (* Holonimy (elementy kolekcji) *)
  blisk = "60" (* bliskoznaczność *) (* Bliskoznaczne z *)
  stan = PlWordnet.rev_relation ("92" (* Stanowość (stanowość V-Adj) *)
  typy = "106" (* Typy *)
  caus = PlWordnet.rev_relation ("124" (* Kauzacja (kauzacja procesu DK-Adj) *)
  caus2 = PlWordnet.rev_relation ("126" (* Kauzacja (kauzacja procesu NDK-Adj) *)
  attr = "145" (* klucz (parent) jest cechą, wartość - wartością cechy *) (* Wartości cechy *)
  fem = "53" (* formy żeńskie *) (* Nazwy żeńskie *)
  demi = "56" (* zdrobnienia *) (* Nacechowanie (deminutywy) *)
  aug = "57" (* zdrobnienia *) (* Nacechowanie ( augmentatywność) *)
  deryw = "59" (* Derywatywy *)
  rel2 = "62" (* Synonimy międzyparadygmatyczne (synonimia międzyparadygmatyczna V-N) *)
  grad = "151" (* Stopniowanie (stopień wyższy) *)
  rel = "169" (* adj -> noun *) (* Synonimy międzyparadygmatyczne (relacyjne) *) (*wanilinowy-2*)
  rel3 = "244" (* adv -> adj *) (* Synonimy międzyparadygmatyczne (Adv-Adj) *)
  let ex_hipero = Xlist.fold [attr; holo; holo2; holo3; blisk; rel; rel2; rel3; (*chr;*) fem; demi; stan; typy; aug; deryw; caus; caus2; grad] hipero PlWordnet.sum_relation in
  let ex_hipo = Xlist.fold [attr; holo; blisk; rel; chr; fem; demi] hipo (fun ex_hipo r -> PlWordnet.sum_relation ex_hipo (PlWordnet.rev_relation r)) in
*)

(*
done relations
lr_151.tab  lr_244.tab  lr_56.tab  lr_62.tab   sr_107.tab  sr_11.tab   sr_126.tab  sr_20.tab  sr_25.tab  sr_60.tab
lr_169.tab  lr_53.tab   lr_59.tab  sr_106.tab  sr_10.tab   sr_124.tab  sr_145.tab  sr_23.tab  sr_28.tab

non struct relations
lr_101.tab  lr_104.tab  lr_13.tab  lr_142.tab  lr_19.tab  lr_63.tab  sr_108.tab
*)

let hipo_roots = [
  "coś", "1", 50;(*"organizm",40*)
  "GERUNDIUM", "1", 50;
  "istota", "1", 50;
  "rezultat", "1", 40;
  "całość", "1", 30;
  "czynność", "1", 30;
  "cecha", "1", 30;
  "zjawisko", "1", 30;
  "materia", "3", 30;
  "część", "3", 20;
  "środek", "1", 20;
  "okoliczności", "1", 20;
  "miejsce", "1", 20;
  "informacja", "1", 20;
  "ilość", "1", 10;
  "ciąg", "4", 10;
  "wydarzenie", "1", 10;
  "proces o charakterze fizykalnym polegający na ruchu", "1", 10;
  "negatywny", "5", 10;
  "pozytywny", "2", 10;
  "przestrzeń", "1", 10;
  "działalność", "1", 10;
  "podmiot", "3", 10;
  "czyn", "1", 10;
  "inny", "3", 10;
  "inny", "1", 10;
  "dobrze", "5", 10;
  "sprecyzowany", "1", 10;
  "niezwykle", "1", 10;
  "mienie", "1", 10;
  "okres", "1", 10;
  "podobny", "1", 10;
  "inaczej", "1", 10;
  "negatywnie", "2", 10;
  "domena", "2", 10;
  "zależność", "3", 10;
  "rzecz", "6", 10;
  "tkanka", "1", 10;
  "właściwość", "2", 10;
  "meritum", "1", 10;
  "zachowanie", "8", 10;
  "rzecz", "2", 10;
  "typ", "1", 10;
  "przedmiot", "4", 10;
  "rzadkość", "1", 10;
  "jakiś", "1", 10;
  "byt", "1", 0;
  "czynności", "1", 10;
  "CZASOWNIK", "1", 500;
  "robić", "1", 40;
  "spowodować", "1", 30;
  "CZASOWNIK ZMIENNOSTANOWY NIETELICZNY", "1", 40;
  "CZASOWNIK TELICZNY", "1", 50;
  "zrobić", "1", 40;
  "powodować", "1", 10;
  "CZASOWNIK CZYNNOŚCIOWY NDK", "1", 30;
  "CZASOWNIK ZDARZENIOWY NDK", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną z wytwarzaniem czegoś", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną ze współżyciem w społeczeństwie", "1", 10;
  "CZASOWNIK oznaczający sytuacje związane z relacjami abstrakcyjnymi", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną z położeniem (relacjami przestrzennymi) lub zmianą położenia (relacji przestrzennych)", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną z posiadaniem", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną ze stanem mentalnym lub emocjonalnym albo reakcją emocjonalno-fizjologiczną", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną ze zjawiskiem fizycznym", "1", 10;
  "CZASOWNIK oznaczający sytuacje związane z łańcuchem przyczynowo-skutkowym", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną z reakcją organizmu lub czynnością fizjologiczną", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną z następstwem czasowym zdarzeń", "1", 10;
  "CZASOWNIK oznaczający sytuację związaną z kontaktem fizycznym", "1", 10;
  "CZASOWNIK STANOWY NDK", "1", 10;
  ]
