(*
 *  ENIAM: Categorial Syntactic-Semantic Parser for Polish
 *  Copyright (C) 2016 Wojciech Jaworski <wjaworski atSPAMfree mimuw dot edu dot pl>
 *  Copyright (C) 2016 Institute of Computer Science Polish Academy of Sciences
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type config =
  {resources_path: string; walenty_filename: string; pre_port: int; pre_host: string;
   results_path: string; max_no_solutions: int; lcg_timeout: float; lcg_no_nodes: int; no_processes: int;
   concraft_enabled: bool; concraft_path: string;
   mate_parser_enabled: bool; mate_parser_path: string;
   swigra_enabled: bool; swigra_path: string;
   sentence_selection_enabled: bool
  }

let empty_config =
  {resources_path="../resources/"; walenty_filename="/Users/Daniel/Desktop/clarin/eniam/walenty/walenty_20160412.xml"; pre_port=3258; pre_host="localhost";
   results_path="../resuls/"; max_no_solutions=10; lcg_timeout=100.; lcg_no_nodes=10000000; no_processes=4;
   concraft_enabled=false; concraft_path="";
   mate_parser_enabled=false; mate_parser_path="";
   swigra_enabled=false; swigra_path="";
   sentence_selection_enabled=true
  }

let config = Xlist.fold (File.load_lines "../config") empty_config
  (fun config s ->
    match Str.split (Str.regexp "=") s with
      ["RESOURCES_PATH";resources_path] -> {config with resources_path=resources_path}
    | ["WALENTY";walenty_filename] -> {config with walenty_filename=walenty_filename}
    | ["PRE_PORT";pre_port] ->{config with pre_port=int_of_string pre_port}
    | ["PRE_HOST";pre_host] -> {config with pre_host=pre_host}
    | ["RESULTS_PATH";results_path] -> {config with results_path=results_path}
    | ["MAX_NO_SOLUTIONS";max_no_solutions] -> {config with max_no_solutions=int_of_string max_no_solutions}
    | ["LCG_TIMEOUT";lcg_timeout] -> {config with lcg_timeout=float_of_string lcg_timeout}
    | ["LCG_NO_NODES";lcg_no_nodes] -> {config with lcg_no_nodes=int_of_string lcg_no_nodes}
    | ["NO_PROCESSES";no_processes] -> {config with no_processes=int_of_string no_processes}
    | ["CONCRAFT_ENABLED";concraft_enabled] -> {config with concraft_enabled=bool_of_string concraft_enabled}
    | ["CONCRAFT_PATH";concraft_path] -> {config with concraft_path=concraft_path}
    | ["MATE_PARSER_ENABLED";mate_parser_enabled] -> {config with mate_parser_enabled=bool_of_string mate_parser_enabled}
    | ["MATE_PARSER_PATH";mate_parser_path] -> {config with mate_parser_path=mate_parser_path}
    | ["SWIGRA_ENABLED";swigra_enabled] -> {config with swigra_enabled=bool_of_string swigra_enabled}
    | ["SWIGRA_PATH";swigra_path] -> {config with swigra_path=swigra_path}
    | ["SENTENCE_SELECTION_ENABLED";sentence_selection_enabled] -> {config with sentence_selection_enabled=bool_of_string sentence_selection_enabled}
    | [] -> config
    | l -> failwith ("invalid config format: " ^ s ^ " $" ^ String.concat "#" l ^ "$"))

let alt_all = "alt1.tab"
let dict_all = "dict1.tab"
let rules_all = "rules1.tab"

(* let resources_path = "../resources/" *)
let sgjp_path = config.resources_path ^ "SGJP/"

(* let walenty_filename = "/usr/share/walenty/walenty.xml" *)

(* UWAGA: w razie wymiany słownika trzeba przekopiować definicję adv(pron),nonch,possp oraz wygenerować fixed.tab *)
let realizations_filename = config.resources_path ^ "Walenty/phrase_types_expand_20150909.txt"

let pre_port = (*3258*)3158
let pre_host = "localhost"
(* let pre_host = "wloczykij" *)
let server_port = (*3259*)3159
let server_host = "localhost"
(* let server_host = "wloczykij" *)

let proper_names_filename = sgjp_path ^ "proper_names_sgjp_polimorf_20151020.tab"
let proper_names_filename2 = config.resources_path ^ "proper_names_20160104.tab"

let subst_uncountable_lexemes_filename = config.resources_path ^ "subst_uncountable.dat"
let subst_uncountable_lexemes_filename2 = config.resources_path ^ "subst_uncountable_stare.dat"
let subst_container_lexemes_filename = config.resources_path ^ "subst_container.dat"
let subst_numeral_lexemes_filename = config.resources_path ^ "subst_numeral.dat"
let subst_time_lexemes_filename = config.resources_path ^ "subst_time.dat"

let rzeczownik_filename = config.resources_path ^ "plWordnet/rzeczownik.tab"
let czasownik_filename = config.resources_path ^ "plWordnet/czasownik.tab"
let przymiotnik_filename = config.resources_path ^ "plWordnet/przymiotnik.tab"
let synsets_filename = config.resources_path ^ "plWordnet/synsets.tab"
let hipero_filename = config.resources_path ^ "plWordnet/hipero.tab"
let predef_filename = config.resources_path ^ "predef_prefs.tab"
let proper_classes_filename = config.resources_path ^ "proper_classes.tab"

let brev_filename = config.resources_path ^ "brev.tab"
let lemma_frequencies_filename = config.resources_path ^ "NKJP1M/NKJP1M-lemma-freq.tab"

let mte_filename = sgjp_path ^ "mte_20151215.tab"
