% -*- prolog -*-
%
% To jest ślad po próbie zrobienia wariantu wielowątkowego w obrębie
% SWI Prologu.  Próba zarzucona, bo nie dało się sensownie nakładać
% limitów pamięciowych i czasowych na wątki.


:-thread_local info/2, input/9, forest/5, fail_goal/2.

:-[swigra2_batch_nkjp].


%:-thread_create(['morph_9.32-s.doa'],X,[]).
%:-thread_create(['morph_9.32-s.doa'],Y,[]).


%:-['morph_9.32-s.doa'].

% w punkcie 8.3.1 dokumentacji jest miły przykład worker-pool
% ale my robimy po swojemu tworząc nowe wątki dla każdego parsowania.
% wiemy, że tworzenie wątku to nie przelewki, ale tak można się nie
% zastanawiać nad czyszczeniem zmiennych globalnych

run_on_files(Wildcard) :-
	message_queue_create(gotowe),
	current_prolog_flag(cpu_count, NThreads),
	expand_file_name(Wildcard, Files),
	length(Files,L), format(user_error,"Jest ~p plików do zanalizowania.~n",L),
	rozpocznij_proces(NThreads, Files, NThreads).

rozpocznij_proces(N,[],NThreads) :- !, Started = NThreads - N, zakończ_proces(Started).
rozpocznij_proces(0,Files,NThreads) :- !, kolejkuj_pliki(Files,NThreads).
rozpocznij_proces(N,[F|FF],NThreads) :- N>0,
%	threads,
	przetwarzaj_plik(F),
	N1 is N-1,
	rozpocznij_proces(N1,FF,NThreads).

kolejkuj_pliki([], NThreads) :- zakończ_proces(NThreads).
kolejkuj_pliki([F|FF], NThreads) :-
	thread_get_message(gotowe,W),
	W = wynik(sent(SampID,SentID,Text),NumTrees,Res),
	format(user_output,"~p\t~p\t~p\t~p\t~p~n", [SampID,SentID,Text,NumTrees,Res]),
%	threads,
	przetwarzaj_plik(F),
	kolejkuj_pliki(FF,NThreads).

zakończ_proces(0) :- !.
zakończ_proces(NThreads) :- NThreads>0,
	thread_get_message(gotowe,W),
	W = wynik(sent(SampID,SentID,Text),NumTrees,Res),
	format(user_output,"~p\t~p\t~p\t~p\t~p~n", [SampID,SentID,Text,NumTrees,Res]),
%	threads,
	N1 is NThreads-1,
	zakończ_proces(N1).
	

przetwarzaj_plik(F) :-
%	format("~p~n",F),
%	format(user_error,'.',[]),
	format(user_error,'~p~n',[F]),
	thread_create(parsuj_zdanie(F), _,
		      [alias(F), detached(true), at_exit(sygnalizuj_specjalne)]).

sygnalizuj_specjalne :-
	thread_self(Id),
	(thread_property(Id,status(exception(S)))
	-> info(sample_id,SampID),
	 info(sent_id,SentID),
	 info(tekst,Text),
	 thread_send_message(gotowe,wynik(sent(SampID,SentID,Text),0,exception(S)))
	;
	 true).

parsuj_zdanie(F) :-
%	random(X), T is 0.2*X, sleep(T),
	file_name_extension(B,'doa',F),
	file_name_extension(B,'forest',Forest),
	setup_and_call_cleanup(
			   open(Forest,write,ForestStream),
			   with_output_to(ForestStream, load_file(F)),
			   close(ForestStream)
			  ),
	info(sample_id,SampID),
	info(sent_id,SentID),
	info(tekst,Text),
	nb_getval(parsing_result,Res),
	(Res = accepted -> nb_getval(number_of_trees,NumTrees) ; NumTrees=0),
%	findall([Var,Val],nb_current(Var,Val),NumTrees),
	thread_send_message(gotowe,wynik(sent(SampID,SentID,Text),NumTrees,Res)).



% this replaces consult/1 as suggested by Jan Wielemaker on the SWI-Prolog list:

load_file(File) :-
        setup_and_call_cleanup(open(File, read, In),
                           (   repeat,
			       read_term(In, T, []),
                               load_data(T),
			       T=end_of_file
                           ),
                           close(In)).

load_data(end_of_file) :- !.
load_data(Term) :-
        ok_term(Term), !,               % left to the user
        assert(Term).
load_data(':-'(X)) :- !,
	call(X).
load_data(Term) :-
	throw(load_data(Term)).
%        domain_error(my_data, Term).    % use meaningful error

ok_term(info(_,_)) :- !.
ok_term(input(_,_,_,_,_,_,_,_,_)) :- !.


% random(X), T is 2*X, sleep(T).

%:-run_on_files('*/*/*.doa').
% swipl -s ~/swigra/parser/birnam_threaded.pl -t "run_on_files('*/*/*.doa')" >zsyp
