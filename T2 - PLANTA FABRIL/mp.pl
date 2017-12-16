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


%-------------------------------PARSE_PROBLEMA------------------------------------------------------
% PREENCHER COM O QUE JÁ TINHAMOS
%---------------------------------------------------------------------------------------------------


%-------------------------------GERADOR_PROBLEMA----------------------------------------------------
% PREENCHER COM O QUE JÁ TINHAMOS
%---------------------------------------------------------------------------------------------------


%--------------------------------MAIN---------------------------------------------------------------
mp(Input_Trabalhos, Input_Recursos, Input_Trabalhadores):-
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
