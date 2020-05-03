% -*- prolog -*-
%
% swipl -s ../parser/walenty2swigra.pl -g "konwertuj('20150417/verbs/walenty_20150417_verbs_all.txt')" -t halt >slowczas.pl
% swipl -s ../parser/walenty2swigra.pl -g "konwertuj('20150629/verbs/walenty_20150629_verbs_all.txt')" -t halt >slowczas.pl
% swipl -s ../parser/walenty2swigra.pl -g "konwertuj_realizacje('/home/marcin/IPI/swigra/walenty/20150417/realizations_20150417.txt')" -t halt

% Dodaję (rejestruj_schemat) takie schematy:
%być	Q	np(bier)+advp
%być	V	np(dop)
% strona bierna:
%być	V	adjp(mian)
%zostać	V	adjp(mian)
% Trzeba tego zaniechać, jeśli Walenty się zmieni.

% :-library(readutil).

schemat([L,v(Sie,Neg,Aspekt),P,Oc]) --> slowo(L), mozesie(Sie), ":",
	" ", ocena(Oc), ":",
	" ", negi(Neg), ":",
	" ", ":",
	" ", aspekt(Aspekt), ":",
	" ", pozycje(P).
schemat([L,nv(Sie,Pd),P,Oc]) --> slowo(L), mozesie(Sie), ":",
	" ", ocena(Oc), ":",
	" ", ":",
	" ", pred(Pd), ":",
	" ", ":",
	" ", pozycje(P).

slowo(S) --> litera(L), litery(LL), { atom_codes(S, [L|LL]) }.

litery([L|LL]) --> litera(L), litery(LL).
litery([]) --> [].

litera(L) --> [L], { code_type(L, alpha) }.
cyfra(L) --> [L], { code_type(L, digit) }.
koniec --> [10].
koniec --> [13,10].

slowa(S) --> litera(L), resztaslow(LL), { atom_codes(S, [L|LL]) }.
resztaslow([L|LL]) --> litera(L), resztaslow(LL).
resztaslow([0' |LL]) --> " ", resztaslow(LL). %' 
resztaslow([0'-|LL]) --> "-", resztaslow(LL). %' 
resztaslow([]) --> [].

identyfikator(I) --> litera(L), znaki_identyfikatorowe(ZZ), { atom_codes(I, [L|ZZ]) }.
znaki_identyfikatorowe([Z|ZZ]) --> [Z], { code_type(Z, csym) }, 
				   znaki_identyfikatorowe(ZZ).
znaki_identyfikatorowe([]) --> [].
lista_srednikowa([S|SS]) --> "[", slowo(S), ogon_listy_srednikowej(SS).
ogon_listy_srednikowej([S|SS]) --> ";", slowo(S), ogon_listy_srednikowej(SS).
ogon_listy_srednikowej([]) --> "]".

% ciąg znaków nie zawierający C:
oprocz(C,[L|LL]) --> [L], {L \= C}, oprocz(C,LL).
oprocz(_C,[]) --> [].

quoted(S) --> "'", oprocz(0'', C), { atom_codes(S, C) }, "'".

duzo_quoted([L|LL]) --> quoted(L), ",", duzo_quoted(LL).
duzo_quoted([L]) --> quoted(L).

% jak duzo, ale rozdzielone średnikami:
srednio_quoted([L|LL]) --> quoted(L), ";", srednio_quoted(LL).
srednio_quoted([L]) --> quoted(L).

mozesie(niesię) --> [].
mozesie(się) --> " ", "się".

ocena(pewny) --> "pewny".
ocena(potoczny) --> "potoczny".
ocena(zły) --> "zły".
ocena(wątpliwy) --> "wątpliwy".
ocena(archaiczny) --> "archaiczny".
ocena(wulgarny) --> "wulgarny".

negi(_Neg) --> "_".
negi( aff) --> "aff".
negi( neg) --> "neg".

pred(nd) --> [].
pred(pred) --> "pred".

aspekt(imperf) --> "imperf".
aspekt(perf)   --> "perf".
aspekt(_Asp)   --> "_".

pozycje([P|PP]) --> pozycja(P), mozepozycje(PP).

mozepozycje([]) --> [].
mozepozycje([P|PP]) --> "+", pozycja(P), mozepozycje(PP).

% To jest używane wyłącznie wewnątrz lex(compar(…)…):
pseudopozycje([P|PP]) --> argument(P), mozepseudopozycje(PP).

mozepseudopozycje([]) --> [].
mozepseudopozycje([P|PP]) --> "+", argument(P), mozepseudopozycje(PP).



pozycja(P) --> atrybutypozycji(P,AA), "{", argumenty(AA), "}".

atrybutypozycji(A,A) --> [].
atrybutypozycji(A^subj,A) --> "subj".
atrybutypozycji(A^obj,A) --> "obj".
atrybutypozycji(A^ctlr,A) --> "controller".
atrybutypozycji(A^ctle,A) --> "controllee".
atrybutypozycji(A^ctlr2,A) --> "controller2".
atrybutypozycji(A^ctle2,A) --> "controllee2".
atrybutypozycji(A^subj^ctlr,A) --> "subj,controller".
atrybutypozycji(A^subj^ctle,A) --> "subj,controllee".
atrybutypozycji(A^obj^ctlr,A) --> "obj,controller".
atrybutypozycji(A^obj^ctle,A) --> "obj,controllee".
atrybutypozycji(A^ctle^ctlr2,A) --> "controllee,controller2".
atrybutypozycji(A^obj^ctle^ctlr2,A) --> "obj,controllee,controller2".

argumenty([A|AA]) --> argument(A), mozeargumenty(AA).

mozeargumenty([]) --> [].
mozeargumenty([A|AA]) --> ";", argument(A), mozeargumenty(AA).

argument_nieleksykalny(np(P)) --> "np(", slowo(P), { przypadek(P) } ,")".
argument_nieleksykalny(adjp(P)) --> "adjp(", slowo(P), { przypadek(P) } ,")".
argument_nieleksykalny(prepnp(S,P)) --> "prepnp(", slowa(S),",",slowo(P),{przypadek(P)}, ")".
argument_nieleksykalny(prepadjp(S,P)) --> "prepadjp(", slowo(S),",",slowo(P),{przypadek(P)}, ")".
argument_nieleksykalny(comprepnp(S)) --> "comprepnp(", slowa(S), ")".
argument_nieleksykalny(cp(S)) --> "cp(", identyfikator(S), { typ_cp(S) }, ograniczenie_cp(S), ")".
argument_nieleksykalny(ncp(P,S)) --> "ncp(",
	slowo(P), {przypadek(P)},
	",",
	identyfikator(S), { typ_cp(S) },
	ograniczenie_cp(S),
	")".
argument_nieleksykalny(prepncp(Pm,P,S)) --> "prepncp(",
	slowa(Pm),
	",",
	slowo(P), {przypadek(P)},
	",",
	identyfikator(S), { typ_cp(S) } ,
	ograniczenie_cp(S),
	")".
argument_nieleksykalny(A) --> "xp(", slowo(S), { typ_xp(S) }, ograniczenie_xp(S,A) ,")".
argument_nieleksykalny(advp(S)) --> "advp(", slowo(S), { typ_advp(S) } ,")".
argument_nieleksykalny(compar(S)) --> "compar(", slowo(S), { typ_compar(S) } ,")".
argument_nieleksykalny(infp(S)) --> "infp(", aspekt(S), ")".
argument_nieleksykalny(fixed(T,S)) --> "fixed(", argument_nieleksykalny(T),
	",", quoted(S), ")".
argument_nieleksykalny(possp) --> "possp".
argument_nieleksykalny(distrp) --> "distrp".
argument_nieleksykalny(refl) --> "refl".
argument_nieleksykalny(recip) --> "recip".
argument_nieleksykalny(nonch) --> "nonch".
argument_nieleksykalny(or) --> "or".
argument_nieleksykalny(e) --> "E".

argument(A) --> argument_nieleksykalny(A).
argument(A) --> argument_leksykalny(A).

argument_leksykalny(lex(compar(S),PP,nomod)) --> "lex(compar(", slowo(S), {typ_compar(S)} ,"),", !,
		pseudopozycje(PP),
		")".
argument_leksykalny(lex(xp(M):compar(J),PP,nomod)) --> "lex(xp(", slowo(M), {typ_xp(M)},
	"[compar(", slowo(J), {typ_compar(J)}, ")]),", !, 
		pseudopozycje(PP),
		")".
argument_leksykalny(lex(A, Par, Mod)) --> "lex(", argument_lexa(A),
	parametry_leksykalne(A,Par),
	",", modyfikatory_leksykalne(Mod),
	")".

% To, co może stać wewnątrz lex(…):
argument_lexa(A) --> argument_nieleksykalny(A).
argument_lexa(prepgerp(S,P)) --> "prepgerp(", slowa(S),",",slowo(P),{przypadek(P)}, ")".
argument_lexa(ppasp(P)) --> "ppasp(", slowo(P), { przypadek(P) } ,")".
argument_lexa(pactp(P)) --> "pactp(", slowo(P), { przypadek(P) } ,")".
argument_lexa(prepnump(S,P)) --> "prepnump(", slowa(S),",",slowo(P),{przypadek(P)}, ")".
argument_lexa(nump(P)) --> "nump(",slowo(P),{przypadek(P)}, ")".
argument_lexa(prepppasp(S,P)) --> "prepppasp(", slowo(S),",",slowo(P),{przypadek(P)}, ")".
argument_lexa(qub) --> "qub".

% Do rozbudowy, jak się pojawi więcej.  No i do zinterpretowania !!!
ograniczenie_xp(T,xp(T)) --> "".
ograniczenie_xp(T,xp(T):Ogr) --> "[", argument_lexa(Ogr), "]".

ograniczenie_cp(_) --> "".
ograniczenie_cp(int) --> lista_srednikowa(Sp). % !!! wykorzystać Sp
ograniczenie_cp(rel) --> lista_srednikowa(Sp). % !!! wykorzystać Sp

przypadek(agr).
przypadek(str).
przypadek(nom).
przypadek(gen).
przypadek(dat).
przypadek(acc).
przypadek(inst).
przypadek(loc).
przypadek(part).
przypadek(pred).
przypadek(postp).

typ_cp(int).
typ_cp(rel).  % !!! nowy z podtypami
typ_cp(aż).
typ_cp(czy).
typ_cp(dopóki).
typ_cp(gdy).
typ_cp(jak).
typ_cp(jakby).
typ_cp(jakoby).
typ_cp(jeśli).
typ_cp(kiedy).
typ_cp(zanim).
typ_cp(że).
typ_cp(żeby).
typ_cp(żeby2).

typ_xp(abl).
typ_xp(adl).
typ_xp(mod).
typ_xp(locat).
typ_xp(dur).
typ_xp(temp).
typ_xp(perl).
typ_xp(caus).
typ_xp(dest).

typ_advp(abl).
typ_advp(adl).
typ_advp(dur).
typ_advp(locat).
typ_advp(misc).
typ_advp(mod). % wystepuje tylko w fixedach?
typ_advp(perl). %
typ_advp(pron).
typ_advp(temp).

typ_compar(jak).
typ_compar(jako).
typ_compar(niż).
typ_compar(co). % !!! mają chyba wyrzucić (dać jako potoczny wariant jak)

parametry_leksykalne(np(_),[Num, Lemma]) --> !,
	",", number(Num),
	",", lematy(Lemma).
parametry_leksykalne(prepnp(_,_),[Num, Lemma]) --> !, 
	",", number(Num),
	",", lematy(Lemma).
parametry_leksykalne(adjp(_),[Num,Gend,Degr,Lemma]) --> !,
	",", number(Num),
	",", gender(Gend),
	",", degree(Degr),
	",", lematy(Lemma).
parametry_leksykalne(infp(_), [Neg,Lemma,Sie]) --> !,
	",", negacja(Neg),
	",", lematy(Lemma),
	",", sie(Sie).
parametry_leksykalne(prepgerp(_,_), [Num,Neg,Lemma,Sie]) --> !,
	",", number(Num),
	",", negacja(Neg),
	",", lematy(Lemma),
	",", sie(Sie).
parametry_leksykalne(xp(_),[]) --> !, {throw('lex(xp) bez ograniczenia')}.
parametry_leksykalne(xp(_):A, P) --> !, parametry_leksykalne(A,P).
%% parametry_leksykalne(compar(_), [lex(A, Par, Mod)]) --> !,
%% 	argument(lex(A, Par, Mod)).
parametry_leksykalne(ppasp(_),[Num,Gend,Neg,Lemma]) --> !,
	",", number(Num),
	",", gender(Gend),
	",", negacja(Neg),
	",", lematy(Lemma).
parametry_leksykalne(pactp(_),[Num,Gend,Neg,Lemma,Sie]) --> !,
	",", number(Num),
	",", gender(Gend),
	",", negacja(Neg),
	",", lematy(Lemma),
	",", sie(Sie).
parametry_leksykalne(advp(_),[Degr,Lemma]) --> !,
	",", degree(Degr),
	",", lematy(Lemma).
parametry_leksykalne(cp(_), [Neg, Lemma, Sie]) --> !,	
	",", negacja(Neg),
	",", lematy(Lemma),
	",", sie(Sie).
parametry_leksykalne(prepnump(_,_), [LemmaNum,LemmaS]) --> !,
	",", lematy(LemmaNum),
	",", lematy(LemmaS).
parametry_leksykalne(nump(_), [LemmaNum,LemmaS]) --> !,
	",", lematy(LemmaNum),
	",", lematy(LemmaS).
parametry_leksykalne(prepadjp(_,_),[Num,Gend,Degr,Lemma]) --> !,
	",", number(Num),
	",", gender(Gend),
	",", degree(Degr),
	",", lematy(Lemma).
parametry_leksykalne(prepppasp(_,_),[Num,Gend,Neg,Lemma]) --> !,
	",", number(Num),
	",", gender(Gend),
	",", negacja(Neg),
	",", lematy(Lemma).
parametry_leksykalne(ncp(_,_), [Neg, Lemma, Sie]) --> !,	
	",", negacja(Neg),
	",", lematy(Lemma),
	",", sie(Sie).
parametry_leksykalne(qub, [Lemma]) --> !,
	",", lematy(Lemma).

parametry_leksykalne(X,_) --> {throw(nieznany_arg_lexa(X))}.

modyfikatory_leksykalne(natr) --> "natr".
modyfikatory_leksykalne(atr(Obl,Mult,Mod)) -->
	obl_atr(Obl),"atr", mult_atr(Mult), "(",
	pozycje(Mod),
	")".
modyfikatory_leksykalne(atr) --> "atr". % to chyba tymczasowe? !!!
modyfikatory_leksykalne(atr1) --> "atr1". % to chyba tymczasowe? !!!
modyfikatory_leksykalne(ratr) --> "ratr". % to chyba tymczasowe? !!!
modyfikatory_leksykalne(ratr1) --> "ratr1". % to chyba tymczasowe? !!!

lematy(Lemma) --> quoted(Lemma).
lematy(L^coor) --> "OR(",
	duzo_quoted(L),
	")".
lematy(L^or) --> "OR(",
	srednio_quoted(L),
	")".
lematy(L^xor) --> "XOR(",
	duzo_quoted(L),
	")".

number(sg) --> "sg".
number(pl) --> "pl".
number(agr) --> "agr".
number(_) --> "_".

gender(agr) --> "agr".
gender(m1) --> "m1".
gender(m2) --> "m2".
gender(m3) --> "m3".
gender(n) --> "n".
gender(f) --> "f".

degree(pos) --> "pos".
degree(com) --> "com".
degree(sup) --> "sup".
degree(_) --> "_".

sie('') --> "".
sie(się) --> "się".

negacja(aff) --> "aff".
negacja(neg) --> "neg".
negacja(_) --> "_".

obl_atr(obl) --> "r".
obl_atr(fac) --> "".

mult_atr(1) --> "1".
mult_atr(n) --> "".

konwertuj(File) :-
% dwa specjalne schematy dla być/nie ma i „książka była Piotra”:
	rejestruj_schemat([być,v(niesię,neg,imperf),[[np(str)],[advp(misc)]],pewna]),
	rejestruj_schemat([być,v(niesię,_,imperf),[[np(str)]^subj,[np(gen)]],pewna]),
% schematy dla strony biernej:
	rejestruj_schemat([być,v(niesię,neg,imperf),[[np(str)]^subj,[adjp(nom)]],pewna]),
	rejestruj_schemat([zostać,v(niesię,neg,imperf),[[np(str)]^subj,[adjp(nom)]],pewna]),
%	trace,
	format('% This is a generated file. Do not modify.\n% Modifications should be applied to the file ~p instead.\n%\n',[File]),
	zassij_plik(File),
% wypisujemy:
	current_key(H),
	findall(Poz,recorded(H,Poz),Schemat),
	format('slowczas(~p,~W).\n',[H,Schemat,[portray(true),quoted(true)]]),
	fail.
konwertuj(_) :-
    format('%%% Local Variables:\n%%% coding: utf-8\n%%% mode: prolog\n%%% End:\n',[]).

zassij_plik(File) :-
	open(File, read, Stream),
	repeat,
	read_line_to_codes(Stream, Codes),
	interpretuj(Codes),
	Codes = end_of_file, !.


interpretuj(end_of_file) :- !.
interpretuj([]) :- !.
interpretuj([0'% | L]) :- !, format('%~s\n',[L]). % '
interpretuj(Codes) :- phrase(schemat(L), Codes),
		      rejestruj_schemat(L), !.
interpretuj(Codes) :- atom_codes(A,Codes), throw(problem(A)).

rejestruj_schemat([_,nv(_,_),_,_]) :- !.
% pomijamy schematy zleksykalizowane:
rejestruj_schemat([_,_,Sch,_]) :- member([lex(_,_,_)|_],Sch), !.
rejestruj_schemat([_,_,Sch,_]) :- member([lex(_,_,_)|_]^_,Sch), !.
rejestruj_schemat([H,v(Sie,Neg,Asp),Sch,Oc]) :-
%    write('%** '), writeln(Sch),
    przetwórz_pozycje(Sch2,Sch),
    ( Sie = się *-> Sch3 = [[sie]|Sch2] ; Sch3 = Sch2),
    recordz(H,Sch3).%% ,
    %% write(H),
    %% ( Sie = się *-> write(' się'); true),
    %% write('\tQ\t'),
    %% write_term(Rozpisany,[portray(true),quoted(true)]),
    %% nl.

przetwórz_pozycje([],[]) :- !.
przetwórz_pozycje([RP|RR],[S|SS]) :-
    jaka_pozycja(S,P,Zn_poz,R,RP),
    przetwórz_argumenty(P,Zn_poz,R),
    przetwórz_pozycje(RR,SS).

jaka_pozycja(S^subj,S,subj,R,subj(R)) :- !.
jaka_pozycja(S^subj^_,S,subj,R,subj(R)) :- !.
jaka_pozycja(S^obj,S,obj,R,R) :- !.
jaka_pozycja(S^obj^_,S,obj,R,R) :- !.
jaka_pozycja(S^_,S,jakaś,R,R).
jaka_pozycja(S,S,jakaś,R,R).

przetwórz_argumenty([],_,[]) :- !.
przetwórz_argumenty([A|AA],Zn_poz,[P|PP]) :-
    przetwórz_argument(A,Zn_poz,P),
    przetwórz_argumenty(AA,Zn_poz,PP).


przetwórz_argument(np(str),subj,np(mian)) :- !.
przetwórz_argument(np(str),_,np(bier)) :- !.
przetwórz_argument(ncp(str,Typ),subj,sentp(mian,Typ)) :- !.
przetwórz_argument(ncp(str,Typ),_,sentp(bier,Typ)) :- !.
% postarzanie:
przetwórz_argument(np(Przyp),_,np(OPrzyp)) :- !, postarz_przypadek(Przyp,OPrzyp).
przetwórz_argument(adjp(Przyp),_,adjp(OPrzyp)) :- !, postarz_przypadek(Przyp,OPrzyp).
przetwórz_argument(prepnp(Pm,str),_,prepnp(Pm,mian)) :- !.
przetwórz_argument(prepnp(Pm,Przyp),_,prepnp(Pm,OPrzyp)) :- !, postarz_przypadek(Przyp,OPrzyp).
przetwórz_argument(prepadjp(Pm,str),_,prepadjp(Pm,mian)) :- !.
przetwórz_argument(prepadjp(Pm,Przyp),_,prepadjp(Pm,OPrzyp)) :- !, postarz_przypadek(Przyp,OPrzyp).
przetwórz_argument(comprepnp(Typ),_,prepnp(Typ,dop)) :- !.
przetwórz_argument(cp(Typ),_,sentp(OTyp)) :- !, postarz_cp(Typ,OTyp).
przetwórz_argument(ncp(Przyp,Typ),_,sentp(OPrzyp,OTyp)) :- !, postarz_przypadek(Przyp,OPrzyp), postarz_cp(Typ,OTyp).
przetwórz_argument(prepncp(Pm,Przyp,Typ),_,sentp(Pm,OPrzyp,OTyp)) :- !, postarz_przypadek(Przyp,OPrzyp), postarz_cp(Typ,OTyp).
przetwórz_argument(infp(Asp),_,infp(OAsp)) :- !, postarz_asp(Asp,OAsp).
przetwórz_argument(xp(_Typ),_,advp) :- !.
przetwórz_argument(advp(_Typ),_,advp) :- !.
przetwórz_argument(e,_,np(mian)) :- !.
przetwórz_argument(refl, _, sie) :- !.
przetwórz_argument(recip, _, sie) :- !.
% :koniec postarzania
przetwórz_argument(A,_,A).

portray('_') :- write('_').

% postarzanie:
postarz_przypadek(agr,agr).
postarz_przypadek(str,str).
postarz_przypadek(nom,mian).
postarz_przypadek(gen,dop).
postarz_przypadek(dat,cel).
postarz_przypadek(acc,bier).
postarz_przypadek(inst,narz).
postarz_przypadek(loc,miej).
postarz_przypadek(part,part).
postarz_przypadek(pred,pred).
postarz_przypadek(postp,pop).

postarz_cp(int,pz) :- !.
postarz_cp(T,T).

postarz_asp(X,'_') :- var(X), !.
postarz_asp(imperf,nd):- !.
postarz_asp(perf,dk) :- !.

konwertuj_realizacje(File) :- 
    read_file_to_codes(File, S, []), 
    phrase(realizacje,S),
%    format('~s',[R]), 
    listing(realizuje), listing(realizuje_typ).

realizacje --> 
    "% Arguments realizations:", koniec,
    realizacje_argumentów, koniec,
    "% Attributes realizations:", koniec,
    realizacje_typów, koniec,
    "% Attributes equivalents:
jak-->
    niczym
przeciw-->
    przeciwko
".

realizacje_argumentów -->
    realizacje_argumentu,
    realizacje_argumentów.
realizacje_argumentów --> [].

realizacje_argumentu --> 
%    {trace},
    argument_nieleksykalny(A), "-->", koniec,
    lista_realizacji_argumentu(A).

lista_realizacji_argumentu(A) -->
    realizacja_argumentu(A),
    lista_realizacji_argumentu(A).
lista_realizacji_argumentu(_) --> [].

realizacja_argumentu(A) -->
    "    ", slowo(R), {\+ member(R, [possp,distrp,refl,nonch,or,'E'])}, "\t[", ocena(O), "]", koniec,
    { assertz(realizuje(A,R,O)) }.
realizacja_argumentu(A) -->
    "    ", argument(R), "\t[", ocena(O), "]", koniec,
    { assertz(realizuje(A,R,O)) }.

realizacje_typów -->
    realizacje_typu,
    realizacje_typów.
realizacje_typów --> [].

realizacje_typu --> 
%    {trace},
    slowo(A), { typ_cp(A) }, "-->", koniec,
    lista_realizacji_typu(A).

lista_realizacji_typu(A) -->
    realizacja_typu(A),
    lista_realizacji_typu(A).
lista_realizacji_typu(_) --> [].

realizacja_typu(A) -->
    "    ", slowo(R), "\t[", ocena(O), "]", koniec,
    { assertz(realizuje_typ(A,R,O)) }.
