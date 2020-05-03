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

open ENIAMtokenizerTypes
open ENIAMlexSemanticsTypes
open Xstd

let subst_inst_time = StringSet.of_list [
  "wiosna";
  "lato";
  "jesień";
  "zima";
  "wieczór";
  ]

let adj_roles = Xlist.fold [
  "ten",               "Coref","";
  "mój",               "Possesive",""; (*"ja"*)
  "twój",              "Possesive",""; (*"ty"*)
  "nasz",              "Possesive",""; (*"my"*)
  "wasz",              "Possesive",""; (*"wy"*)
  "swój",              "Possesive","";
  "czwartkowy",        "Time","";
  "dawny",             "Time","";
  "ubiegłotygodniowy", "Time","";
  (* operators:  nielokalnie zmieniaja formułe logiczna *)
  "każdy",             "Quantifier","";
  "wszelki",           "Quantifier","";
  "wszystek",          "Quantifier","";
  "żaden",             "Quantifier",""; (*"każdy"*)
  (* semi operators:  operatory równoważne domyślnemu \exists *) (* FIXME: jak je zaklasyfikować? *)
  "jakiś",             "Quantifier","";
  "pewien",            "Quantifier","";
  "niektóry",          "Quantifier","";
  "jedyny",            "Quantifier","";
  (* not operators *)
(*   "sam",               "Attribute",(["ArgWNadrzedniku";"RelacjaWNadrzedniku"],TConst("local",[])); *)
  "sam",               "Quantifier",""; (* nie wprowadza zmiennej, w znaczeniu "Jan sam ugotował obiad", "zupa z samych marchewek" bo wymaga odwołania się do agenta i zdarzenia *)
(* tego sie da odroznic od poprzedniego "sam", LConcept "Attribute"); (* albo pragmatyczne podkreślenie "w samym centrum", albo "dokładnie" - "w samo południe" albo  *) *)
(*   "jedyny",            "Attribute",(["ArgWNadrzedniku";"RelacjaWNadrzedniku"],TConst("local",[])); (* nie wprowadza zmiennej *)  *)
  "jaki",              "Attribute","";
  "jeden",             "Quantifier",""; (* jeden - kilka to kwantyfikator z 3 polami; Bóg jeden = Bóg sam; miara lioczebności *)
  "taki",              "Attribute","";
  "czyj",              "Possesive","";
  "który",             "Attribute","";
  ] StringMap.empty (fun map (k,r,a) -> StringMap.add_inc map k [r,a] (fun l -> (r,a) :: l))

let adv_roles = Xlist.fold [ (* FIXME: problem z podwójnymi przypisaniami *)
  (* operators:  nielokalnie zmieniaja formułe logiczna *)
  "codziennie",     "Quantifier","";
  "zawsze",         "Quantifier","";
  "nigdy",          "Quantifier",""; (*"zawsze",*)
  "prawdopodobnie", "OpModal","";
  "razem",          "Quantifier","";  (* czy to jest operator? kolektywny - zakazuje dystrybutywności akcji i innych jej podrzęników *)
  (* "chyba", LIndexical "Unk");  - otwiera nowy model, to jest kublik *)
  (* not operators *)
  "nieraz",         "Quantifier",""(*[],TConst("approx",[])*);  (* odpowiedznik liczby mnogiej dla czasownika *)
  "tymczasem",      "Time","";
  "zawczasu",       "Time","";
  "ostatnio",       "Time",""; (*"czas, o który trzeba zmodyfikować moment wypowiedzenia"*)
  "niedługo",       "Duration",""; (* Jak to rozróznić ?? *)
  "dlaczego",       "Condition","";
  "jak",            "Attribute","";
  "dlatego",        "Condition",""; (* odniesieniem argumentu jest sytuacji/kontekst *)
  "tak",            "Manner",""; (* odniesieniem argumentu jest sytuacji/kontekst, byc może deiktyczny *)

(*  "skąd",		"Location","Source";
  "skądkolwiek",	"Location","Source";
  "skądś",		"Location","Source";
  "skądże",		"Location","Source";
  "stamtąd",		"Location","Source";
  "stąd",		"Location","Source";
  "zewsząd",		"Location","Source";
  "znikąd",		"Location","Source";
  "skądinąd",		"Location","Source";
  "skądciś",		"Location","Source";
  "skądsiś",		"Location","Source";
  "blisko",		"Location","Goal";
  "daleko",		"Location","Goal";
  "dokąd",		"Location","Goal";
  "dokądkolwiek",	"Location","Goal";
  "donikąd",		"Location","Goal";
  "gdzie",		"Location","Goal";
  "gdziekolwiek",	"Location","Goal";
  "gdzieś",		"Location","Goal";
  "gdzież",		"Location","Goal";
  "niedaleko",		"Location","Goal";
  "tam",		"Location","Goal";
  "tamże",		"Location","Goal";
  "tu",			"Location","Goal";
  "tutaj",		"Location","Goal";
  "wstecz",		"Location","Goal";
  "długo",		"Duration","";
  "ile",		"Duration","";
  "ilekolwiek",		"Duration","";
  "jeszcze",		"Duration","";
  "już",		"Duration","";
  "krótko",		"Duration","";
  "nadal",		"Duration","";
  "trochę",		"Duration","";
  "tyle",		"Duration","";
  "wciąż",		"Duration","";
  "ciągle",		"Duration","";
  "blisko",		"Location","";
  "daleko",		"Location","";
  "dokoła",		"Location","";
  "dookoła",		"Location","";
  "gdzie",		"Location","";
  "gdziekolwiek",	"Location","";
  "gdzieniegdzie",	"Location","";
  "gdzieś",		"Location","";
  "gdzież",		"Location","";
  "naokoło",		"Location","";
  "niedaleko",		"Location","";
  "nieopodal",		"Location","";
  "nigdzie",		"Location","";
  "nisko",		"Location","";
  "obok",		"Location","";
  "opodal",		"Location","";
  "ówdzie",		"Location","";
  "poniżej",		"Location","";
  "powyżej",		"Location","";
  "tam",		"Location","";
  "tamże",		"Location","";
  "tu",			"Location","";
  "tutaj",		"Location","";
  "wewnątrz",		"Location","";
  "wkoło",		"Location","";
  "wokoło",		"Location","";
  "wokół",		"Location","";
  "wszędzie",		"Location","";
  "wysoko",		"Location","";
  "naokół",		"Location","";
  "wszędy",		"Location","";
  "którędy",		"Path","";
  "którędykolwiek",	"Path","";
  "którędyś",		"Path","";
  "którędyż",		"Path","";
  "tamtędy",		"Path","";
  "tędy",		"Path","";
  "owędy",		"Path","";
  "kędy",		"Path","";
  "kędykolwiek",	"Path","";
  "często",		"Time","";
  "częstokroć",		"Time","";
  "dawno",		"Time","";
  "dotąd",		"Time","";
  "dotychczas",		"Time","";
  "dzisiaj",		"Time","";
  "dziś",		"Time","";
  "jutro",		"Time","";
  "kiedy",		"Time","";
  "kiedykolwiek",	"Time","";
  "kiedyś",		"Time","";
  "nieczęsto",		"Time","";
  "niedawno",		"Time","";
  "niedługo",		"Time","";
  "niegdyś",		"Time","";
  "niekiedy",		"Time","";
  "nieraz",		"Time","";
  "nieregularnie",	"Time","";
  "nierzadko",		"Time","";
  "obecnie",		"Time","";
  "ówcześnie",		"Time","";
  "podówczas",		"Time","";
  "pojutrze",		"Time","";
  "popojutrze",		"Time","";
  "potem",		"Time","";
  "późno",		"Time","";
  "przedwczoraj",	"Time","";
  "przenigdy",		"Time","";
  "regularnie",		"Time","";
  "rzadko",		"Time","";
  "sporadycznie",	"Time","";
  "teraz",		"Time","";
  "wcześnie",		"Time","";
  "wczoraj",		"Time","";
  "wkrótce",		"Time","";
  "wnet",		"Time","";
  "wówczas",		"Time","";
  "wtedy",		"Time","";
  "wtenczas",		"Time","";
  "zaraz",		"Time","";
  "zawsze",		"Time","";
  "drzewiej",		"Time","";
  "latoś",		"Time","";
  "naonczas",		"Time","";
  "naówczas",		"Time","";
  "natenczas",		"Time","";
  "nikędy",		"Time","";
  "ninie",		"Time","";
  "onegdaj",		"Time","";
  "ongi",		"Time","";
  "ongiś",		"Time","";
  "wczas",		"Time","";
  "wonczas",		"Time","";*)
  ] StringMap.empty (fun map (k,r,a) -> StringMap.add_inc map k [r,a] (fun l -> (r,a) :: l))

let qub_roles = Xlist.fold [
  "tylko",          "Quantifier","";
  "jeszcze",        "Quantifier","";
  "już",            "Quantifier","";
  "i",              "Quantifier","";
  "również",        "Quantifier","";
  "także",          "Quantifier","";
  "też",            "Quantifier","";
  "znowu",          "Quantifier","";
  "może",           "OpModal","";
  "oczywiście",     "OpModal","";
  "zapewne",        "OpModal","";
  "aż",             "OpModal","";
  "dopiero",        "OpModal","";
  "nawet",          "OpModal","";
  "jednak",         "OpModal","";
(*   "zresztą",        "Theme",""; *)
  "zbyt",           "Mod","";
  "dość",           "Manner","";
  "ani",            "Mod","";
  "około",          "Mod","";
  "ponad",          "Mod","";
  "prawie",         "Mod","";
  "przynajmniej",   "Mod","";
  ] StringMap.empty (fun map (k,r,a) -> StringMap.add_inc map k [r,a] (fun l -> (r,a) :: l))


let prep_roles = Xlist.fold [ (* lemma,case,role,role_attr,sense/hipero,sel_prefs *)
  "od","gen",		"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "spod","gen",		"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "spomiędzy","gen",	"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "sponad","gen",	"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "spopod","gen",	"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "spośród","gen",	"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "spoza","gen",	"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "sprzed","gen",	"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "z","gen",		"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "z","postp",		"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "znad","gen",		"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "zza","gen",		"Location","Source",["POŁOŻENIE"],["POŁOŻENIE"];
  "do","gen",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "ku","dat",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "między","acc",	"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "na","acc",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "na","postp",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "nad","acc",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "nieopodal","gen",	"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "opodal","gen",	"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "pod","acc",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "ponad","acc",	"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"]; (* dodane *)
  "pomiędzy","acc",	"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "poza","acc",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "przed","acc",	"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "w","acc",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "za","acc",		"Location","Goal",["POŁOŻENIE"],["POŁOŻENIE"];
  "dzięki","dat",	"Condition","",["CZEMU"],["ALL"];
  "na","acc",		"Condition","",["CZEMU"],["ALL"];
  "na","postp",		"Condition","",["CZEMU"],["ALL"];
  "od","gen",		"Condition","",["CZEMU"],["ALL"];
  "przez","acc",	"Condition","",["CZEMU"],["ALL"];
  "wskutek","gen",	"Condition","",["CZEMU"],["ALL"];
  "z","gen",		"Condition","",["CZEMU"],["ALL"];
  "dla","gen",		"Purpose","",["CZEMU"],["ALL"];
  "do","gen",		"Purpose","",["CZEMU"],["ALL"];
  "ku","dat",		"Purpose","",["CZEMU"],["ALL"];
  "na","acc",		"Purpose","",["CZEMU"],["ALL"];
  "na","postp",		"Purpose","",["CZEMU"],["ALL"];
  "po","acc",		"Purpose","",["CZEMU"],["ALL"];
  "do","gen",		"Duration","",["CZAS"],["CZAS"];
  "od","gen",		"Duration","",["CZAS"],["CZAS"];
  "przez","acc",	"Duration","",["CZAS"],["CZAS"];
  "dokoła","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "dookoła","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "koło","gen",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "między","inst",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "nad","inst",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "na","loc",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "na","postp",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "naokoło","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "naprzeciw","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "naprzeciwko","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "nieopodal","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "obok","gen",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "opodal","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pod","inst",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "po","loc",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pomiędzy","inst",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "ponad","inst",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "poniżej","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "popod","inst",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pośrodku","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pośród","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "powyżej","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "poza","inst",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "przed","inst",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "przy","loc",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "u","gen",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "vis-à-vis","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wewnątrz","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wkoło","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "w","loc",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wokół","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wśród","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wzdłuż","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "za","inst",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "naokół","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pobocz","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pobok","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "podal","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "podle","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "śród","gen",		"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wedle","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wpośród","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wśrzód","gen",	"Location","",["POŁOŻENIE"],["POŁOŻENIE"];
  "po","postp",		"Manner","",["ALL"],["ALL"];
  "bez","gen",		"Manner","",["ALL"],["ALL"];
(*  "jako","str",	"Manner","",[],[];
  "jak","str",		"Manner","",[],[];*)
  "pod","acc",		"Manner","",["ALL"],["ALL"];
  "z","inst",		"Manner","",["ALL"],["ALL"];
  "z","postp",		"Manner","",["ALL"],["ALL"];
  "dokoła","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "dookoła","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "koło","gen",		"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "między","inst",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "nad","inst",		"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "naokoło","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "obok","gen",		"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pod","inst",		"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "po","loc",		"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "pomiędzy","inst",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "ponad","inst",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "poprzez","acc",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "przez","acc",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "via","nom",		"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wokoło","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wokół","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wzdłuż","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "naokół","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "około","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "skroś","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wskroś","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "wskróś","gen",	"Path","",["POŁOŻENIE"],["POŁOŻENIE"];
  "koło","gen",		"Time","",["CZAS"],["CZAS"];
  "lada","nom",		"Time","",["CZAS"],["CZAS"];
  "między","inst",	"Time","",["CZAS"],["CZAS";"SYTUACJA"];
  "około","gen",	"Time","",["CZAS"],["CZAS"];
  "o","loc",		"Time","",["CZAS"],["CZAS"];
  "podczas","gen",	"Time","",["CZAS"],["CZAS";"SYTUACJA"];
  "po","loc",		"Time","",["CZAS"],["CZAS";"SYTUACJA"];
  "pomiędzy","inst",	"Time","",["CZAS"],["CZAS";"SYTUACJA"];
  "przed","inst",	"Time","",["CZAS"],["CZAS";"SYTUACJA"];
  "w","acc",		"Time","",["CZAS"],["CZAS"];
  "w","loc",		"Time","",["CZAS"],["CZAS"];
  "temu","acc",		"Time","",["CZAS"],["CZAS"]; (* dodane *)
  "za","gen",		"Time","",["CZAS"],["CZAS"]; (* dodane *)
  "o","acc",		"Arg","",["ALL"],["ALL"]; (* FIXME: zaślepka *)
  ] StringMap.empty (fun map (lemma,case,role,role_attr,hipero,sel_prefs) ->
      let hipero = Xlist.map hipero (fun hipero -> ENIAMwalTypes.Predef hipero) in
      let sel_prefs = Xlist.map sel_prefs (fun sel_prefs -> ENIAMwalTypes.Predef sel_prefs) in (* FIXME: było SynsetName, dlaczego??? *)
      StringMap.add_inc map lemma [case,role,role_attr,hipero,sel_prefs]
        (fun l -> (case,role,role_attr,hipero,sel_prefs) :: l))
(*  "przeciwko","dat","Dat";
  "przeciw","dat","Dat";
  "o","acc","Theme";
  "o","loc","Theme";
  "według","gen","Manr";
  "wobec","gen","Dat";*)


let noun_sem_args = Xlist.fold [
  "jutro",        ["indexical"];(*"dzień"*)
  "pojutrze",     ["indexical"];(*"dzień"*)
  "cóż",          ["interrogative"];
  "przyszłość",   ["indexical"]; (*"czas"*)
  "raz",          []; (* FIXME: pojemnik, występuje jako adjunct *)
(*   "połowa",       ["approx"]; (* FIXME: pojemnik oraz pojemnik na czas *) *)
  "coś",    [];
  "nic",    [];(*"wszystko"*) (*"organiczenie zakresu"*)
  "nikt",   [];(*"wszyscy"*) (*"organiczenie zakresu"*)
  "co",     ["interrogative"];
  "kto",    ["interrogative"];
  "to",     ["deictic"];
  "cokolwiek",    [];
  "ktokolwiek",   [];
  "ktoś",         [];
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let adj_sem_args = Xlist.fold [
(*  "1935",              "=",[],"name",[]); (*"rok"*)
  "1998",              "=",[],"name",[]); (*"rok"*)
  "25",                "=",[],"name",[]); (*"dzień"*)
  "XIX",               "=",[],"name",[]); (*"wiek"*)
  "Oświęcimski",       "=",[],"name",[]);*)
  "pierwszy",          ["order"(*relacja porządkująca*)]; (* mamy zadan sekwencję (czyli zbiór z porządkiem), jest to niejawny argument predykatu pierwszy, mamy też obiekt ktory ma mieć ceche pierwszości, rzeczownik nadrzedny do przymiotnika pierwszy  *)
  "drugi",             ["order"(*relacja porządkująca*)]; (* jw *)
  "ostatni",           ["order"(*relacja porządkująca*)]; (* jw *)
  "kolejny",           ["order"(*relacja porządkująca*)]; (* jw + domyślny argument z poprzednikiem *)
  "inny",              ["coreferential"]; (* niejawny argument ze zbiorem dotychczas rozpartywanych obiektów *)
  "ten",               ["deictic";"coreferential"];
  "mój",               ["indexical"]; (*"ja"*)
  "twój",              ["indexical"]; (*"ty"*)
  "nasz",              ["indexical"(*; "zbiór indywiduów"*)]; (*"my"*)
  "wasz",              ["indexical"(*; "zbiór indywiduów"*)]; (*"wy"*)
  "swój",              ["coreferential"];
  "czwartkowy",        [](*"name"*); (*"dzień tygodnia"*) (* ewentualnie indexical *)
  "dawny",             ["indexical";"comparative"(*miara*)];(*"czas"*) (*"czas, októry trzeba zmodyfikować moment wypowiedzenia"*)
  "ubiegłotygodniowy", ["indexical"];(*"czas"*)
  (* unk *)
  "jaki",              ["interrogative"];
  "taki",              ["indexical"];
  "czyj",              ["interrogative"];
  "który",            ["interrogative"];(* FIXME: dodać relative *)
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let adv_sem_args = Xlist.fold [
  "tymczasem",      ["coreferential"(*czas wypowiedzenia*)];(*"czas"*)
  "wtedy",          ["coreferential"(*czas wypowiedzenia*)];(*"czas"*)
  "wówczas",        ["coreferential"(*czas wypowiedzenia*)];(*"czas"*)
  "zawczasu",       ["coreferential"(*czas wypowiedzenia*)];(*"czas"*)
  "teraz",          ["indexical"(*czas wypowiedzenia*)];(*"czas"*)
  "zaraz",          ["indexical"(*czas wypowiedzenia*)];(*"czas"*)
  "dziś",           ["indexical"(*czas wypowiedzenia*)];(*"dzień"*)
  "niedługo",       ["indexical"(*czas wypowiedzenia*);"comparative"(*miara*)];(*"czas"*) (*"czas, o który trzeba zmodyfikować moment wypowiedzenia"*)
  "ostatnio",       ["indexical"(*czas wypowiedzenia*);"comparative"(*miara*)];(*"czas"*) (*"czas, o który trzeba zmodyfikować moment wypowiedzenia"*)
  "wkrótce",        ["indexical"(*czas wypowiedzenia*);"comparative"(*miara*)];(*"czas"*) (*"czas, o który trzeba zmodyfikować moment wypowiedzenia"*)
  "dawno",          ["indexical"(*czas wypowiedzenia*);"comparative"(*miara*)];(*"czas"*) (*"czas, o który trzeba zmodyfikować moment wypowiedzenia"*)
  "niedługo",       ["comparative"(*miara*)];(*"czas"*) (* Jak to rozróznić ?? *)
  "długo",          ["comparative"(*miara*)];(*"czas"*) (* pojęcie kontekstu jeszcze ogóniejsze niż przy zawczasu *)
  "późno",          [(*"Context";*)"comparative"(*miara*)];(*"czas"*) (* albo indexical albo foric albo concept *)
  "stąd",           ["indexical"];(*"miejsce"*)
  "obok",           ["coreferential"];(*"miejsce"*)
  "tam",            ["coreferential";"deictic"];(*"miejsce"*)
  "tu",             ["indexical"];(*"miejsce"*)
  "gdzieś",         [];
(*   "sam", ArgWNadrzedniku "Quant"; (* nie wprowadza zmiennej, w znaczeniu "Jan sam ugotował obiad", bo wymaga odwołania się do agenta i zdarzenia *)  *)
  "dlaczego",       ["interrogative"];
  "jak",            ["interrogative"];
  "kiedy",          ["interrogative"];
  "dlatego",        ["coreferential"]; (* odniesieniem argumentu jest sytuacji/kontekst *)
  "tak",            ["coreferential"]; (* odniesieniem argumentu jest sytuacji/kontekst, byc może deiktyczny *)
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let qub_sem_args = Xlist.fold [
  "tylko",          []; (* przyrematyczny (wskazuje remat) *)
  "jeszcze",        ["order"(*relacja porządkująca*)]; (* reprezentacja: określamy obiekt ktory jest w skali i stwierdzamy że istnieje inny obiekt wcześniej w skali, który spełnia ten sam event w tej samej roli *) (* operator: restrykcja zadana przez remat, zakres przez temat; semantycznie: isnieje porządek, sąd jest prawdziwy dla pewnego obiektu mniejszego w porządku i dla aktualnego obiektu ; pragmatycznie: mówca spodziewa się, że sąd nie jest prawdziwy dla elementu większego w porządku (np. przestanie być w późniejszym momencie) *)
  "już",            ["order"(*relacja porządkująca*)]; (* reprezentacja: określamy obiekt ktory jest w skali i stwierdzamy że istnieje inny obiekt wcześniej w skali, który nie spełnia tego eventu w tej roli *) (* dualny do jeszcze *)
  "i",              [](*["topic_syn";"focus_syn"]*); (* w znaczeniu również *)
  "również",        [](*["topic_syn";"focus_syn"]*); (* musi mieć za argument również temat *) (* semantyka istnieje inny remat, który spełnia temat *)
  "także",          [](*["topic";"focus"]*);
  "też",            [(*"topic";"focus"*)];
  "znowu",          []; (* kwantyfikacja po razach *) (* działa na formule logicznej - wcześniej działo się cos takiego samego *)
  "może",           [(*"topic";"focus"*)](*,"focus",[])*); (* remat jest w kontekście modalnym, a temat poza nim *) (* operator definiujący rolę epistemiczną dla formuły *)
  "oczywiście",     [(*"topic";"focus";*) "indexical"(*speaker*)](*,"focus",[])*); (* nie wprowadza modelu ale traktujemy go analogicznie, bo to co oczywiste pozostaje prawdziwe *) (* operator definiujący rolę epistemiczną dla formuły, otwiera kontekst identyczny z zewnętrznym *)
  "zapewne",        [(*"topic";"focus";*) "indexical"(*speaker*)](*,"focus",[])*); (* analogicznie do może *) (* operator definiujący rolę epistemiczną dla formuły *)
  "aż",             [(*"topic_syn";"focus_syn";*) "indexical"(*speaker*)](*,"syntax",[])*); (* stosunek mówcy do treści rematu *)(* przyrematyczny (wskazuje remat) *) (* dla mówiącego jest dziwne, że nie mniej; modyfikator rematu (sądu ustrukturyzowanego tematycznie) *)
  "dopiero",        [(*"topic_syn";"focus_syn";*) "indexical"(*speaker*)](*,"syntax",[])*); (* przyrematyczny (wskazuje remat) *)
  "nawet",          [(*"topic_syn";"focus_syn";*) "indexical"(*speaker*)](*,"syntax",[])*); (* przyrematyczny (wskazuje remat) *)
  "jednak",         [(*"topic";"focus";*) "indexical"; "coreferential"(*podmiot epistemiczny*)](*,"focus",[])*);
  "zresztą",        ["coreferential"];
  "zbyt",           []; (* ew. Attr *) (* "comparative"(*miara*) wnoszony przez przymiotnik *)
  "dość",           [];
  "ani",            [];
  "około",          [];
  "ponad",          [];
  "prawie",         [];
  "przynajmniej",   [];
  "się",["coreferential"];
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let pron_sem_args = Xlist.fold [
  "ja",    ["indexical"]; (* elementy zdarzenia komunikacyjnego; wyrażenie okazjonalne; kontekst komunikacji oznaczamy przez "indexical" *)
  "my",    ["indexical"(*; "zbiór indywiduów"*)];
  "pro1",  ["indexical"(*; "zbiór indywiduów"*)];
  "ty",    ["indexical"];
  "wy",    ["indexical"(*; "zbiór indywiduów"*)];
  "pro2",  ["indexical"(*; "zbiór indywiduów"*)];
  "on",    ["coreferential";"deictic"]; (* phoric: wystepuje wczesniej w tekście; deictic: określony przez niewerbalny element komunikacji *)
  "ona",   ["coreferential";"deictic"];
  "ono",   ["coreferential";"deictic"];
  "oni",   ["coreferential";"deictic"];
  "one",   ["coreferential";"deictic"];
  "on-p1", ["coreferential";"deictic"];
  "on-p2", ["coreferential";"deictic"];
  "on-p3", ["coreferential";"deictic"];
  "pro3"  ,["coreferential";"deictic"];
  "pro3sg",["coreferential";"deictic"];
  "pro3pl",["coreferential";"deictic"];
  "pro",   ["indexical";"coreferential";"deictic"];
  "siebie",["coreferential"];
  ] StringMap.empty (fun map (k,l) -> StringMap.add map k l)

let reversed_hipero = Xlist.fold [
  "ppron12","ja";
  "ppron12","my";
  "ppron12","ty";
  "ppron12","wy";
  "ppron3","on";
  "siebie","siebie";
  "qub","się";
  "subst","co";
  "subst","kto";
(*  "","";
  "","";
  "","";
  "","";
  "","";
  "","";*)
  ] StringMap.empty (fun map (pos,lemma) ->
    StringMap.add_inc map pos (StringSet.singleton lemma) (fun set -> StringSet.add set lemma))
