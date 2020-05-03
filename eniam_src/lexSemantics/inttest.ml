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


let test_strings = [
  (* "Szpak frunie zimą."; *)
  (* "Kot miauczy w październiku."; *)
  (* "Arabia Saudyjska biegnie."; *)
  "Chłopcy mają ulicę kwiatami.";
  (* "Kot miałczy."; *)
  (* "Np. Ala.";
  "Kot np. miauczy.";
  "Szpak frunie. Kot miauczy.";
  "Szpak powiedział: „Frunę. Kiszę.”";
  "W XX w. Warszawa.";
     "Teraz frunie jakiś szpak."; *)
(*  "a gdybym miałem";
  "A Gdy Miałem";
  "GDY MIAŁEM";
  "I II III IV V VI VII VIII IX X MCXIV MXC";
  "Kiedy Piotr Prabucki, przewodniczący Komisji Budżetu PeKaO";
  "25 idzie 20.";
  "Kot. Kot. kot.";
  "25.";
  "25.888.231";
  "Ala 25.888.231.111 ma.";
  "Ala 25.888.031,011.";
  "Ala -25.888.031,011.";
  "Ala -25 .";
  "Ala -1° C  3° ciepła 20—30°C od 180° do 260°C  około 6° poniżej horyzontu.";
  "Ala 22-25 .";
  "Ala 22.5.2000-25.5.2001 .";*)
(*  "Np. Ala.";*)
  (* "w. dom.";
  "tzn.";
  "c.d.n."; *)
(*  "Cauchy'ego ONZ-owska biegnie.";*)
  (* "TE-cie E-e.";
  "MS-DOS-owska CI-cie KRRi-cie UJ-ocie UJ-OCIE.";
  "rock'n'rollowy d’Alembertowi staro-cerkiewno-słowiańskimi"; *)
(*  "Tom idzie.";*)
  (* "Miałem miał."; *)
(*  "Szpak śpiewa.";
  "Ala ma kota.";
  "Ale mają kota:"*)
  ]

let _ =
  ENIAMpreIntegration.concraft_enabled := true;
  ENIAMpreIntegration.mate_parser_enabled := true;
  ENIAMlexSemantics.initialize ();
  ENIAMpreIntegration.initialize ();
  print_endline "Testy wbudowane";
  Xlist.iter test_strings (fun s ->
    print_endline ("\nTEST: " ^ s);
    let text,tokens = ENIAMsubsyntax.parse_text s in
    let text = ENIAMpreIntegration.parse_text ENIAMsubsyntaxTypes.Struct tokens text in
    let lex_sems = ENIAMlexSemantics.assign tokens text in
    print_endline (ENIAMsubsyntaxStringOf.text "" tokens text);
    print_endline "";
    print_endline (ENIAMsubsyntaxStringOf.token_extarray tokens);
    print_endline "";
    print_endline (ENIAMlexSemanticsStringOf.string_of_lex_sems tokens lex_sems));
(*  print_endline "Testy użytkownika.";
  print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
  let s = ref (read_line ()) in
  while !s <> "" do
    let tokens = ENIAMtokenizer.parse !s in
    (* print_endline (ENIAMtokenizer.xml_of tokens); *)
    Xlist.iter tokens (fun token -> print_endline (ENIAMtokenizer.string_of 0 token));
    print_endline "Wpisz tekst i naciśnij ENTER, pusty tekst kończy.";
    s := read_line ()
  done;*)
  ENIAMpreIntegration.stop_server !ENIAMpreIntegration.concraft_server_pid;
  ()
