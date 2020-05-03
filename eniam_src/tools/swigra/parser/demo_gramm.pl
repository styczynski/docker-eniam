:-style_check(-singleton).
expr(A, B, C, D) :-
	goal(terminal(43), B, E, F),
	goal(term(G), E, H, I),
	J is D+G,
	register(expr(J), A, H, e1, [C/expr(D), B, F/terminal(43), E, I/term(G)], K),
	expr(A, H, K, J).

expr(A, B, C, D) :-
	goal(terminal(45), B, E, F),
	goal(term(G), E, H, I),
	J is D-G,
	register(expr(J), A, H, e2, [C/expr(D), B, F/terminal(45), E, I/term(G)], K),
	expr(A, H, K, J).

term(A, B, C, D) :-
	register(expr(D), A, B, e3, [C/term(D)], E),
	expr(A, B, E, D).

term(A, B, C, D) :-
	goal(terminal(42), B, E, F),
	goal(factor(G), E, H, I),
	J is D*G,
	register(term(J), A, H, t1, [C/term(D), B, F/terminal(42), E, I/factor(G)], K),
	term(A, H, K, J).

term(A, B, C, D) :-
	goal(terminal(47), B, E, F),
	goal(factor(G), E, H, I),
	J is D/G,
	register(term(J), A, H, t2, [C/term(D), B, F/terminal(47), E, I/factor(G)], K),
	term(A, H, K, J).

factor(A, B, C, D) :-
	register(term(D), A, B, t3, [C/factor(D)], E),
	term(A, B, E, D).

number(A, B, C, D) :-
	register(factor(D), A, B, f1, [C/number(D)], E),
	factor(A, B, E, D).

number(A, B, C, D) :-
	goal(digit(E), B, F, G),
	H is 10*D+E,
	register(number(H), A, F, n2, [C/number(D), B, G/digit(E)], I),
	number(A, F, I, H).

terminal(A, B, C, 40) :-
	goal(expr(D), B, E, F),
	goal(terminal(41), E, G, H),
	register(factor(D), A, G, f2, [C/terminal(40), B, F/expr(D), E, H/terminal(41)], I),
	factor(A, G, I, D).

terminal(A, B, C, D) :-
	D>=48,
	D=<57,
	E is D-48,
	register(digit(E), A, B, d1, [C/terminal(D)], F),
	digit(A, B, F, E).

digit(A, B, C, D) :-
	register(number(D), A, B, n1, [C/digit(D)], E),
	number(A, B, E, D).

