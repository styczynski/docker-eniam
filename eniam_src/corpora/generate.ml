(*
 *  ENIAMcorpora is a library that integrates ENIAM with corpora in CONLL format
 *  Copyright (C) 2016 Daniel Oklesinski <oklesinski dot daniel atSPAMfree gmail dot com>
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

(* Generowanie pliku info_sentences.txt na podstawie skladnicy walencyjnej*)
let _ =
  (*ConllParser.processSkladnica () *)
  ()

(* Generowanie pliku info_sentences2.txt na podstawie krzaków *)
let _ =
  (* Resources.info_file () *)
  ()

(* Generowanie pliku ../../NLP resources/krzaki_interp_statistics.txt na podstawie krzaków *)
let _ =
  (* InterpsInCorpus.print_diagnose () *)
  ()
