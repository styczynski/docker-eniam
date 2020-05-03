% -*- prolog -*-

% To ze wzgledu na Windows, gdzie UTF-8 nie wydaje sie byc domyslny:
:-encoding(utf8).
:-set_prolog_flag(encoding,utf8).


%:-use_module(library(http/xpce_httpd)).
:-use_module(library(http/thread_httpd)).
:-use_module(library(http/http_dispatch)).
:-use_module(library(http/http_parameters)).
:-use_module(library(http/html_write)).

:-html_set_options([dialect(xhtml)]).

% Potrzebne moduły Birnama i Świgry:
:-[
   birnam_run,
   birnam_dumpedges,
   forest2xml,
   gfjp_morfologia,
   gfjp_slowskr,
   gfjp_slowpart,
   slowczas, slowrzecz, slowprzym, slowwykrz,
   gfjp_analiza,
   gfjp2_wymagania,
   birnam_sequences,
   gfjp2_warunki,
   gfjp2,
   gfjp2_arnoscint,
   gfjp_swidzinskify
].

% moduł dumpedges pisze do pliku, potrzebujemy to zmienić, żeby od
% razu poddać wynik transformacji do XML-a:

:-abolish(dumpterm/1).
dumpterm(porazka) :- !.
dumpterm(T) :-
	assertz(T).

parsuj(T) :-
	analiza(T),
	statprint(sent_id,no_id), % forest2xml wymaga identyfikatora zdania
	open('./httpd/forest.xml',write,XMLStream,[encoding(utf8)]),
	forest2xml(XMLStream),
	close(XMLStream),
	retractall(known_identifier(_,_,_,_)),
	abolish(edge/6),
	abolish(info/2).

%with_output_to(string(A),(set_stream(current_output,encoding(utf8)),f2x)).

% zaszłość ze Świgry 1, której już nie potrzebujemy:
:-abolish(dodajprzec/3).
dodajprzec(_Od, _Do, _Add).


% Predykat odpowiadający na przychodzące do serwera żądanie /swigra :
run_świgra(Request) :-
	http_parameters(Request,
	[q(Text, [default('Czytali.'), length =< 200])]
        ),
%	świgruj(Text),
	catch(świgruj(Text),morf(nieznane,Ign), generuj_przeprosiny_morf(Text,Ign)),
	retractall(known_identifier(_,_,_,_)),
	abolish(edge/6),
	abolish(info/2).

świgruj(Text) :-
    analiza(Text),
    statprint(sent_id,no_id), % forest2xml wymaga identyfikatora zdania
    open('./httpd/forest.xml',write,XMLStream,[encoding(utf8)]),
    forest2xml(XMLStream),
    close(XMLStream),
    shell('cd ..; python -m disambiguator-pcfg.disamb-tree parser/httpd/forest.xml'),
    generate_the_page(Text).

generuj_przeprosiny_morf(Text,Ign) :-
    length(Ign, Ilu),
    (Ilu > 1 *->
	     format(atom(Przepro), 'Niestety nie znam ~p słów: ~p.', [Ilu, Ign])
     ;
	     format(atom(Przepro), 'Niestety nie znam słowa „~p”.', Ign)
    ),
    reply_html_page(
	    [title('Świgra: ~p' - [Text]),
	     link([ href('swigra.css'), rel(stylesheet), type('text/css')], [])
	    ],
        body([],
	     [ center(table([width('98%'), border(0 ),
		      cellpadding(0 ), cellspacing(2 )],
		     [tr(td([bgcolor('#26264f'),height(14)],
			    [])),
		      tr(td([bgcolor('#c3d0e4'),height(24)],
			    [p(class(tytul),
			       ['Analizator składniowy ', em('Świgra 2')])])),
		      tr(td([bgcolor('#26264f'),height(7)], [])),
		      tr(td([ 
%% p('W poniższym polu prosimy wpisać kompletne zdanie do analizy (z uwzględnieniem znaków interpunkcyjnych).'),
			      \generate_query_form(Text)
			      ])),
		      tr(td([bgcolor('#26264f'),height(5)],
			    [])),
		      p([],Przepro),
		      p([],'W sumie mógłbym zgadywać, ale wyniki będą pewnie lepsze, jeśli użyjesz innego słowa o tej samej charakterystyce.'),
		      tr(td([bgcolor('#778899'),height(24),align(right)],
		      'Copyright © 2007–2016 by Instytut Podstaw Informatyki PAN')),
		      tr(td([bgcolor('#26264f'),height(7)],
			    []))
		     ]))
    ])).    


% Generowanie szkieletu strony WWW:

generate_the_page(Text) :-
	reply_html_page(
	[title('Świgra: ~p' - [Text]),
	 link([ href('swigra.css'), rel(stylesheet), type('text/css')], []),
	 script([ type('application/javascript'), src('viewer.js') ],[]),
	 script([ type('application/javascript'), src('viewer_helpers.js') ],[]),
	 script([ type('application/javascript')], \ ['<![CDATA[
function init() {
','
	var viewer = new forestviewer(document.getElementById("viewer"));
','
	viewer.helper = tree_helpers;
	viewer.style = "simple";
	viewer.variablewidth = true;
	viewer.nativescroll = true;
','
	viewer.location = "forest-disamb.xml";
	viewer.display.height=500;
        viewer.display.style.position = "relative";
}]]>']),
	 link([ href('viewer.css'), rel(stylesheet), 
	        type('text/css')], [])
        ],
        body([onload('init()')],
	     [ center(table([width('98%'), border(0 ),
		      cellpadding(0 ), cellspacing(2 )],
		     [tr(td([bgcolor('#26264f'),height(14)],
			    [])),
		      tr(td([bgcolor('#c3d0e4'),height(24)],
			    [p(class(tytul),
			       ['Analizator składniowy ', em('Świgra 2')])])),
		      tr(td([bgcolor('#26264f'),height(7)], [])),
		      tr(td([ 
%% p('W poniższym polu prosimy wpisać kompletne zdanie do analizy (z uwzględnieniem znaków interpunkcyjnych).'),
			      \generate_query_form(Text)
			      ])),
		      tr(td([bgcolor('#26264f'),height(5)],
			    [])),
		      div([id(viewer)],[]),
		      div([id(generalinfo)],[]),
		      \generate_statistics,
		      tr(td([bgcolor('#778899'),height(24),align(right)],
		      'Copyright © 2007–2016 by Instytut Podstaw Informatyki PAN')),
		      tr(td([bgcolor('#26264f'),height(7)],
			    []))
		     ]))
    ])).

%% To jest za nowe (można kiedyś spróbować przywrócić, wtedy będzie
%% można użyć normalnego reply_html_page powyżej):
%% (Ale właściwie nie ma powodu, bo okazało się, że atrybut da się dodać. Zob.wyż.)
%% :- multifile
%%         user:body//2.
%%
%% user:body(_, Body) -->
%%         html(body([onload('init()')], div(Body))).


% Formularz pozwalający wpisać zdanie:

generate_query_form(Text) -->
	html( form( ['accept-charset'('UTF-8'),
		     action('swigra'), method('get')],
		    [p([ input( [size( 80 ),
				 name(q),
				 value(Text)],
				[]),
			 ' ',
			 input( [type(submit),
				 value('Analizuj')],
				[])
			])
		     ])
	      ).

% Część statystyczna strony:

generate_statistics -->
	{ info(parse_inferences, Infer),
	info(parse_cputime, CPU),
	info(trees, Trees),
	info(edges,Edges),
	(info(useful_edges,Useful)-> true; Useful=0) },
	html(table([tr([td('Drzew:'),td(Trees)]),
  	      tr([td('Czas analizy (s):'),td(CPU)]),
  	      tr([td('Krawędzi:'),td(Edges)]),
  	      tr([td('Użytecznych krawędzi:'),td(Useful)]),
	      tr([td('Kroków wywodu:'),td(Infer)])
	  ])
      ).

:- http_handler('/', run_świgra, []).
:- http_handler('/index.html', run_świgra, []).
:- http_handler('/swigra', run_świgra, [prefix]).

:- http_handler('/swigra.css', http_reply_file('httpd/swigra.css',[]), []).

:- http_handler('/forest.xml', http_reply_file('httpd/forest.xml',[]), []).
:- http_handler('/forest-disamb.xml', http_reply_file('httpd/forest-disamb.xml',[]), []).
:- http_handler('/viewer.xhtml', http_reply_file('httpd/viewer.xhtml',[]),[]).
:- http_handler('/viewer.css', http_reply_file('httpd/viewer.css',[]), []).
:- http_handler('/viewer.js', http_reply_file('httpd/viewer.js',[]), []).
:- http_handler('/viewer_helpers.js',
	http_reply_file('httpd/viewer_helpers.js',[]), []).
:- http_handler('/img/gtk-right.png',
	http_reply_file('httpd/gtk-right.png',[]), []).
:- http_handler('/img/gtk-left.png',
	http_reply_file('httpd/gtk-left.png',[]), []).
:- http_handler('/img/pinned.png',
	http_reply_file('httpd/pinned.png',[]), []).
:- http_handler('/img/unpinned.png',
	http_reply_file('httpd/unpinned.png',[]), []).

:- Port = 3333,
	http_server(http_dispatch, [ port(Port) ]),
	format(
"~N************************************************************************

                 Świgra 2 gotowa do pracy pod adresem:

	                  http://localhost:~a/swigra

		      Skieruj tam swojego Firefoksa.", [Port]),
	format("

************************************************************************
").
