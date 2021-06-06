:- use_module(library(clpfd)).
:- use_module(library(simplex)).
:- use_module(library(clpb)).
:- use_module(library(dcg/high_order)).
:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(random)).


% Fatos
x_terminou_depois_de_y(tadeu_torres, luana_lessa).
x_terminou_depois_de_y(tadeu_torres, bruna_barros).
x_terminou_depois_de_y(simone_saraiva, paulo_pereira).
x_terminou_depois_de_y(kaio_kiefer, paulo_pereira).
x_terminou_depois_de_y(kaio_kiefer, miguel_moraes).
x_terminou_depois_de_y(kaio_kiefer, tadeu_torres).
x_terminou_depois_de_y(luana_lessa, bruna_barros).
x_terminou_depois_de_y(luana_lessa, davi_dantas).
x_terminou_depois_de_y(miguel_moraes, simone_saraiva).
x_terminou_depois_de_y(miguel_moraes, bruna_barros).
x_terminou_depois_de_y(davi_dantas, simone_saraiva).
x_terminou_depois_de_y(joana_jensen, paulo_pereira).
x_terminou_depois_de_y(joana_jensen, davi_dantas).
x_terminou_depois_de_y(helena_hansen, luana_lessa).
x_terminou_depois_de_y(helena_hansen, joana_jensen).
x_terminou_depois_de_y(helena_hansen, tadeu_torres).
% Afirmacoes de antes
x_terminou_antes_de_y(tadeu_torres, miguel_moraes).
x_terminou_antes_de_y(paulo_pereira, davi_dantas).
x_terminou_antes_de_y(paulo_pereira, luana_lessa).
x_terminou_antes_de_y(simone_saraiva, joana_jensen).
x_terminou_antes_de_y(simone_saraiva, helena_hansen).
x_terminou_antes_de_y(luana_lessa, joana_jensen).
x_terminou_antes_de_y(luana_lessa, miguel_moraes).
x_terminou_antes_de_y(bruna_barros, joana_jensen).
x_terminou_antes_de_y(bruna_barros, miguel_moraes).
x_terminou_antes_de_y(bruna_barros, paulo_pereira).
x_terminou_antes_de_y(davi_dantas, kaio_kiefer).
x_terminou_antes_de_y(davi_dantas, tadeu_torres).
x_terminou_antes_de_y(joana_jensen, kaio_kiefer).
x_terminou_antes_de_y(joana_jensen, tadeu_torres).
x_terminou_antes_de_y(joana_jensen, miguel_moraes).
x_terminou_antes_de_y(helena_hansen, miguel_moraes).

terminou_antes_de(X,Y) :- 
    x_terminou_antes_de_y(X,Y).

terminou_antes_de(X,Y) :- 
    x_terminou_antes_de_y(X,A),
    x_terminou_antes_de_y(A,B),  
    terminou_antes_de(B,Y).

antes_de(X, L):-
    setof(Y, terminou_antes_de(X, Y), L).    

depois_de(X, L):-
    setof(Y, terminou_depois_de(X, Y), L).    


terminou_depois_de(X,Y) :- 
    x_terminou_depois_de_y(X,Y).

terminou_depois_de(X,Y) :- 
    x_terminou_depois_de_y(X,A),
    x_terminou_depois_de_y(A,B),  
    terminou_depois_de(B,Y).

s_lista_de_part([],0).
s_lista_de_part([_|T], N) :- 
    s_lista_de_part(T, N1), 
    N is N1 + 1.

posicao(X, P):-
    (depois_de(X, B)->true; Po = 0),
    s_lista_de_part(B,Po),
    (antes_de(X, C)->true; P1 = 0),
    s_lista_de_part(C,P1),
    P #> 0,
    P #> Po,
    P #< (11 - P1),
    P #< 11.

melhor_posicao_possivel(X, P):-
    (depois_de(X, B)->true; Po = 0),
    s_lista_de_part(B,Po),
    P is Po + 1.

pior_posicao_possivel(X, P):-
    (antes_de(X, B)->true; Po = 0),
    s_lista_de_part(B,Po),
    P is 10 - Po.

l_ordenada([]).
l_ordenada([A]).
%l_ordenada([A,B]).:- terminou_antes_de(A,B).
l_ordenada([A, B| T]):-
	terminou_antes_de(A,B),
	l_ordenada([B|T]).
   
	
ordem_de_chegada:-   Nomes=[tadeu_torres,luana_lessa,simone_saraiva,paulo_pereira,kaio_kiefer,miguel_moraes,bruna_barros,davi_dantas,joana_jensen,helena_hansen],
  Cs =[KK,PP,SS,DD,LL,JJ,TT,HH,MM,BB],
  Cs ins 1..10,
  all_different(Cs),
  melhor_posicao_possivel(tadeu_torres, A),
  TT #>= A,
  pior_posicao_possivel(tadeu_torres, B),
  TT #=< B,
  %melhor_posicao_possivel(luana_lessa, C),
  %LL #>= C,
  pior_posicao_possivel(luana_lessa, D),
  LL #=< D,
  melhor_posicao_possivel(simone_saraiva, E),
  SS #>= E,
  pior_posicao_possivel(simone_saraiva, F),
  SS #=< F,
  melhor_posicao_possivel(paulo_pereira, G),
  PP #>= G,
  pior_posicao_possivel(paulo_pereira, H),
  PP #=< H,
  melhor_posicao_possivel(kaio_kiefer, I),
  KK #>= I,
  pior_posicao_possivel(kaio_kiefer, J),
  KK #=< J,
  melhor_posicao_possivel(miguel_moraes, K),
  MM #>= K,
  pior_posicao_possivel(miguel_moraes, L),
  MM #=< L,
  melhor_posicao_possivel(bruna_barros, M),
  BB #>= M,
  pior_posicao_possivel(bruna_barros, N),
  BB #=< N,
  melhor_posicao_possivel(davi_dantas, O),
  DD #>= O,
  pior_posicao_possivel(davi_dantas, P),
  DD #=< P,
  melhor_posicao_possivel(joana_jensen, Q),
  JJ #>= Q,
  pior_posicao_possivel(joana_jensen, R),
  JJ #=< R,
  melhor_posicao_possivel(helena_hansen, S),
  HH #>= S,
  pior_posicao_possivel(helena_hansen, T),
  HH #=< T,
  labeling([ff], Cs), write(Cs).
  %label(Cs),
  %write(Cs),nl,fail; true.


ordem_de_chegada_2:-

    Nomes=[tadeu_torres,luana_lessa,simone_saraiva,paulo_pereira,kaio_kiefer,miguel_moraes,bruna_barros,davi_dantas,joana_jensen,helena_hansen],
  Cs =[KK,PP,SS,DD,LL,JJ,TT,HH,MM,BB],
  Cs ins 1..10,
  all_different(Cs),
  posicao(tadeu_torres, TT), 
  posicao(luana_lessa, LL),
  posicao(simone_saraiva, SS),
  posicao(paulo_pereira, PP),
  posicao(kaio_kiefer, KK),
  posicao(miguel_moraes, MM),
  posicao(bruna_barros, BB),
  posicao(davi_dantas, DD),
  posicao(joana_jensen, JJ),
  posicao(helena_hansen, HH),
  labeling([down, min], Cs), 
  write(Cs).



ordem_de_chegada_3:-

Nomes=[kaio_kiefer, paulo_pereira,simone_saraiva,davi_dantas,luana_lessa,joana_jensen,tadeu_torres,helena_hansen,miguel_moraes,bruna_barros],
  Cs =[KK,PP,SS,DD,LL,JJ,TT,HH,MM,BB],
  %append(Cs, Nomes),
  %Cs ins 1..10,
  all_distinct(Cs),
  posicao(tadeu_torres, TT), 
  posicao(luana_lessa, LL),
  posicao(simone_saraiva, SS),
  posicao(paulo_pereira, PP),
  posicao(kaio_kiefer, KK),
  posicao(miguel_moraes, MM),
  posicao(bruna_barros, BB),
  posicao(davi_dantas, DD),
  posicao(joana_jensen, JJ),
  posicao(helena_hansen, HH),
  labeling([min, down], Cs), write(X),
  format("Corredores  : ~q\n",[Nomes]),
  format("Ordem de Chegada: ~q\n",[Cs]).
  %foreach(I in 1..10, [C],(C @= Nomes[Cs[I]],format("Posicao ~d: ~w\n",[I, Cs]))).










