:-use_module(library(clpfd)).
:-use_module(library(timeout)).
:-use_module(library(lists)).
:-use_module(library(random)).

%----------------------------TESTES-----------------------------------------------------------------
teste1_t(
  %trabalho(ID, LISTA_TAREFAS)
              %tarefa(ID, DURACAO, LISTA_RECURSOS_A_USAR, PRECEDENCIAS)
  [trabalho(1,[tarefa(1, 3, [2,2,0], []),
               tarefa(2, 4, [10,20,1], [1]),
               tarefa(3, 5, [5,5,2], [])]) %, trabalho(2, ...etc...)
               ]).

          % maquina(QT_RECURSO, OPERADORES_NECESSARIOS, MASQUARA_BINARIA)
teste1_r([ maquina(10,1,[1,0,0]),
           maquina(20,2,[1,1,0])
           maquina(20,0,[0,0,0]) ]).

teste1_o(operadores([10,5,4])).

%---------------------------------------------------------------------------------------------------


%-------------------------------GERADOR_PROBLEMA----------------------------------------------------
% PREENCHER COM O QUE JÁ TINHAMOS
%---------------------------------------------------------------------------------------------------


%-------------------------------PARSE_PROBLEMA------------------------------------------------------
% PREENCHER COM O QUE JÁ TINHAMOS

% parse_recursos(Lista_Input, Lista_Output)
parse_recursos([],[]).
parse_recursos([maquina(Recursos, Operadores, Mascara)|Resto_In],
               [cumulative(Recursos)|Resto_Out]):-
                 parse_recursos(Resto_In, Resto_Out).

%---------------------------------------------------------------------------------------------------


%--------------------------------MAIN---------------------------------------------------------------
mp(Input_Trabalhos, Input_Recursos, Input_Operadores):-
  % SHOW INPUT TO THE PROBLEM
  write('Input trabalhos:'), nl, write(Input_Trabalhos), nl, nl,
  write('Input recursos:'), nl, write(Input_Recursos), nl, nl,
  write('Input operadores:'), nl, write(Input_Operadores), nl, nl,

  reset_timer,
  % PARSE AND SHOW OUTPUT TO USE
  parse_recursos(Input_Recursos, Output_Recursos_Limites).
  write('Output recursos:'), nl, write(Output_Recursos_Limites), nl, nl,


  write('Tempo de preparacao:'), nl, print_time, nl,
  reset_timer,

  % labeling([minimize(Max_End), bisect, ffc, time_out(5000, _)], Lista_Tempos_Final),

  write('Tempo resolucao:'), nl, print_time, nl,

  % PREENCHER COM O QUE JÁ TINHAMOS
  write('Statistics:'), nl,
  fd_statistics.

%---------------------------------------------------------------------------------------------------


%-----------------------AUXILIAR--------------------------------------------------------------------
reset_timer :- statistics(walltime,_).
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10),
	nl, write('Time: '), write(TS), write('ms'), nl, nl.

con :- consult('mp.pl').

recon :-  reconsult('mp.pl').

reload :- reconsult('C:/Users/edusw/OneDrive/Documents/GitHub/plog_f1/T2 - PLANTA FABRIL/mp.pl').

clr:- write('\33\[2J').
%---------------------------------------------------------------------------------------------------
