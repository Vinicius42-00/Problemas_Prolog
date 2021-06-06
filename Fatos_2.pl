:- use_module(library(clpfd)).
:- use_module(library(simplex)).
:- use_module(library(clpb)).
:- use_module(library(dcg/high_order)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).
:- use_module(library(dif)).

% fatos dado pelo problema
pway(a, b, 10).
pway(b, c, 15).
pway(d, c, 5).
pway(d, b, 10).

% Esboco de solucao


solve(X, , [X], 0).

solve(X, Y, P, N):- 
	(pway(X, Y, N)->true;
	pway(Y, X, N)),
	P = [X, Y].

solve(X,Y,P,N):-
  caminhos_possiveis(X,L), L=[H|T], H = Y, P=[X,Y], pway(X,Y,N);
  solve(H,Y,P1,N1), P=[X|P1], pway(X,H,N0), N is N0+N1.

caminhos_possiveis(X, L) :- 
    	pway(X,Y,_),
	L = Y.

caminhos_possiveis(X, L) :- 
    	pway(Y,X,_),
	L = Y.

caminhos_possiveis(X, L) :- 
    	(pway(X,Y,_)->false;
	pway(Y1,X,_)),
	L = Y1.


todos_caminhos_possiveis(X, L):-
    setof(Y, caminhos_possiveis(X, Y), L). 

limites(X, L):-
	todos_caminhos_possiveis(X, L).
	

origem_destino(S,D):-
	dif(S,D),
	origem_destino_(S,D,[]).

origem_destino_(D,D,_Visitado).
origem_destino_(S,D, Visitado):-
	maplist(dif(S), Visitado),
	limites(S, L1),
	member(X, L1),
	origem_destino_(X,D,[S|Visitado]).

rotas_de_para(X,Y,P):-
	setof(N,origem_destino(X,N), L1),
	P = [X|L1].

	








	