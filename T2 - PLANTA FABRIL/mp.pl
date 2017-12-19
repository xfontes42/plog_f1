:-use_module(library(clpfd)).
:-use_module(library(timeout)).
:-use_module(library(lists)).
:-use_module(library(random)).

%----------------------------TESTES-----------------------------------------------------------------

%----------------------------TESTE 1----------------------------------------------------------------
teste1_t(
  %trabalho(ID, LISTA_TAREFAS)
              %tarefa(ID, DURACAO, LISTA_RECURSOS_A_USAR, PRECEDENCIAS)
  [trabalho(1,[tarefa(1, 3, [2,2,0], []),
               tarefa(2, 4, [10,20,1], [1]),
               tarefa(3, 5, [5,5,2], [])]) %, trabalho(2, ...etc...)
               ]).

          % maquina(QT_RECURSO, OPERADORES_NECESSARIOS, MASQUARA_BINARIA)
teste1_r([ maquina(10,1,[1,0,0]),
           maquina(20,2,[1,1,0]),
           maquina(20,0,[0,0,0]) ]).
teste1_o([10,3,4]).
teste1 :- teste1_t(X), teste1_r(Y), teste1_o(Z), mp(X, Y, Z).
%---------------------------------------------------------------------------------------------------

%---------------------------------------TESTE 2-----------------------------------------------------
teste2_t(
  %trabalho(ID, LISTA_TAREFAS)
              %tarefa(ID, DURACAO, LISTA_RECURSOS_A_USAR, PRECEDENCIAS)
  [trabalho(1,[tarefa(1, 3, [2,2,1,0], []),
               tarefa(2, 4, [10,20,0,1], [1]),
               tarefa(3, 5, [5,5,0,2], [])]),
   trabalho(2, [tarefa(1, 10, [0,0,1,2], []),
                tarefa(2, 1, [4,8,12,7], [1]),
                tarefa(3, 3, [3,5,6,2], [])]) % trabalho(3, etc)
               ]).

          % maquina(QT_RECURSO, OPERADORES_NECESSARIOS, MASQUARA_BINARIA)
teste2_r([ maquina(10,1,[0,1,0,0]),
           maquina(20,2,[1,1,0,0]),
           maquina(15,1,[0,0,0,1]),
           maquina(20,0,[0,0,0,0]) ]).
teste2_o([3,10,4,20]).
teste2 :- teste2_t(X), teste2_r(Y), teste2_o(Z), mp(X, Y, Z).
%---------------------------------------------------------------------------------------------------

%------------------------------------TESTE 3--------------------------------------------------------
teste3_t(
  %trabalho(ID, LISTA_TAREFAS)
              %tarefa(ID, DURACAO, LISTA_RECURSOS_A_USAR, PRECEDENCIAS)
  [trabalho(1,[tarefa(1, 3, [2,2,1,0], []),
               tarefa(2, 4, [10,20,0,1], [1]),
               tarefa(3, 5, [5,5,0,2], [])]),
   trabalho(2, [tarefa(1, 10, [0,0,1,2], []),
                tarefa(2, 1, [4,8,12,7], [1]),
                tarefa(3, 3, [3,5,6,2], [])]) % trabalho(3, etc)
               ]).

          % maquina(QT_RECURSO, OPERADORES_NECESSARIOS, MASQUARA_BINARIA)
teste3_r([ maquina(10,1,[1,0,0,0]),
           maquina(20,2,[1,1,0,0]),
           maquina(15,1,[0,0,0,1]),
           maquina(20,0,[0,0,0,0]) ]).
teste3_o([10,3,4,20]).
teste3 :- teste3_t(X), teste3_r(Y), teste3_o(Z), mp(X, Y, Z).
%---------------------------------------------------------------------------------------------------

teste4_t(
  %trabalho(ID, LISTA_TAREFAS)
              %tarefa(ID, DURACAO, LISTA_RECURSOS_A_USAR, PRECEDENCIAS)
  [trabalho(1,[tarefa(1, 3, [2,2,1,0,10], []),
               tarefa(2, 4, [10,20,0,1,2], [1]),
               tarefa(3, 5, [5,5,0,2,0], [])]),
   trabalho(2, [tarefa(1, 10, [0,0,1,2,9], []),
                tarefa(2, 1, [4,8,12,7,5], [1]),
                tarefa(3, 3, [3,5,6,2,2], [1])]) % trabalho(3, etc)
               ]).
          % maquina(QT_RECURSO, OPERADORES_NECESSARIOS, MASQUARA_BINARIA)
teste4_r([ maquina(10,1,[1,0,0,0]),
           maquina(20,2,[1,1,0,0]),
           maquina(15,1,[0,0,0,1]),
           maquina(20,0,[0,0,0,0]),
           maquina(15,3,[1,0,0,1]) ]).
teste4_o([10,3,4,20]).
teste4 :- teste4_t(X), teste4_r(Y), teste4_o(Z), mp(X, Y, Z).
%---------------------------------------------------------------------------------------------------


%-------------------------------GERADOR_PROBLEMA----------------------------------------------------
% PREENCHER COM O QUE JÁ TINHAMOS
%---------------------------------------------------------------------------------------------------


%-------------------------------PARSE_PROBLEMA------------------------------------------------------
% PREENCHER COM O QUE JÁ TINHAMOS

% parse_recursos_maquina(Lista_Input, Lista_Output)
parse_recursos_maquina([],[],[]).
parse_recursos_maquina([maquina(Recursos, Operadores, Mascara)|Resto_In],
                       [cumulative(Recursos)|Resto_Out],
                       [Operadores-Mascara|Resto_Out_2]):-
                         parse_recursos_maquina(Resto_In, Resto_Out, Resto_Out_2).

% parse_recursos_operador(Lista_Input, Lista_Output)
parse_recursos_operador([],[]).
parse_recursos_operador([Recurso|Resto_In], [cumulative(Recurso)|Resto_Out]):-
                         parse_recursos_operador(Resto_In, Resto_Out).


% output_Result(Lista_Tasks)
output_Result([]):-
 nl,write('End of Tasks'),nl.
output_Result([Head|Lista_Tarefas]) :-
 output_Task(Head),
 output_Result(Lista_Tarefas).


% output_Task(Lista_Tasks)
output_Task(task(Oi, _Duracao, Ei, _Recursos, Ti)) :-
 Trabalho is div(Ti,1000),
 Task is Ti mod 1000,
 write('Trabalho: '),
 write(Trabalho),
 write('      '),
 write('Tarefa: '),
 write(Task),
 write(' started at '),
 write(Oi),
 write(' and ended at '),
 write(Ei),nl.


% parse_duracoes(Lista_Tasks, Lista_Duracoes)
parse_duracoes([], []).
parse_duracoes([task(_,Dur,_,_,_)|Tail], [Dur|Tail2]):-
 parse_duracoes(Tail, Tail2).


% get_start_end(Lista_Tempos, Lista_Inicios, Lista_Fins)
get_start_end([], [], []).
get_start_end([H1-H2|Tail], [H1|Tail2], [H2|Tail3]):-
 get_start_end(Tail, Tail2, Tail3).


% parse_precedencias(ID_Trab, ID_Task, Lista_Prec_In, Lista_Prec_Out)
parse_precedencias(_, _, [], []).
parse_precedencias(ID_Trabalho, Ti ,[H|Tail], [H2-Ti |Tail2]):-
  H2 #= ID_Trabalho * 1000 + H,
  parse_precedencias(ID_Trabalho, Ti,Tail, Tail2).


% enforce_binary_mask(Lista_Vars, Mask)
enforce_binary_mask([],[]).
enforce_binary_mask([Var|Rest], [0|Rest_Mask]):-
  Var #= 0,
  enforce_binary_mask(Rest, Rest_Mask).
enforce_binary_mask([_Var|Rest], [1|Rest_Mask]):-
  enforce_binary_mask(Rest, Rest_Mask).


% output_recursos_operadores_listas(Lista_Recursos, Lista_Rec_Aux, Lista_Operadores)
output_recursos_operadores_listas([], [], []).
output_recursos_operadores_listas([0|Rest_Rec], [_Nop-Mask|Rest_Rec_Aux],
                                                [Lista_Zeros|Lista_Rec_Operadores]):-
  length(Mask, Size_Mask),
  create_list(Size_Mask, 0, Lista_Zeros),
  output_recursos_operadores_listas(Rest_Rec, Rest_Rec_Aux, Lista_Rec_Operadores).
output_recursos_operadores_listas([_Recurso|Rest_Rec], [Nop-Mask|Rest_Rec_Aux],
                                                       [Lista_Op|Lista_Rec_Operadores]):-
  length(Mask, Size_Mask),
  length(Lista_Op, Size_Mask),
  domain(Lista_Op, 0, Nop),
  enforce_binary_mask(Lista_Op, Mask),
  sum(Lista_Op, #=, Nop),
  output_recursos_operadores_listas(Rest_Rec, Rest_Rec_Aux, Lista_Rec_Operadores).


% sum_vertical_elements_in_list(Lista_Listas_Rec, Lista_Somas_Verticais)
sum_vertical_elements_in_list([], []).
sum_vertical_elements_in_list([Lista|Rest_Lista], [Valor|Rest]):-
  sum(Lista, #=, Valor),
  sum_vertical_elements_in_list(Rest_Lista, Rest).


% parse_recursos_task(Recursos_Task, Recursos_Operadores, Resultado)
parse_recursos_task(Recursos, Recurso_Aux, Recursos_Operadores):-
  output_recursos_operadores_listas(Recursos, Recurso_Aux, Lista_Rec_Operadores),
  transpose(Lista_Rec_Operadores, Lista_Transposta),
  sum_vertical_elements_in_list(Lista_Transposta, Recursos_Operadores).
  % append(Recursos, Recursos_Operadores, Recursos_Final).


% parse_tarefas(ID, Lista_Tarefas, Lista_Tasks, Lista_Vars_Dominio, Lista_Precedencias, Rec_Aux)
parse_tarefas(_, [], [], [], [], _).
parse_tarefas(ID, [tarefa(ID_Tarefa, Duracao, Recursos, Precedencias)|Rest],
                  [task(Oi, Duracao, Ei, Recursos-Recursos_Operadores, Ti)|Rest_Task],
                  [Oi-Ei|Variaveis_Dominio_Trabalho],
                  [Precedencias_Final|Precedencias_Trabalho],
                  Recurso_Aux):-
  Ti #= ID * 1000 + ID_Tarefa,
  parse_recursos_task(Recursos, Recurso_Aux, Recursos_Operadores),
  parse_precedencias(ID, Ti, Precedencias, Precedencias_Final),
  parse_tarefas(ID, Rest, Rest_Task, Variaveis_Dominio_Trabalho, Precedencias_Trabalho, Recurso_Aux).


% parse_trabalhos(Lista_Trabalhos, Out_Tarefas, Out_Vars_Dominio, Out_Precedencias, Recursos_Aux)
parse_trabalhos([], [], [], [], _).
parse_trabalhos([trabalho(ID,Tarefas_Trabalho)|Rest],
                     [Tarefas_Trabalho_Final|Lista_Tarefas],
                     [Variaveis_Dominio_Trabalho|Lista_Variaveis_Dominio],
                     [Precedencias_Trabalho|Lista_Precedencias],
                     Recursos_Aux):-
    parse_tarefas(ID,
                  Tarefas_Trabalho,
                  Tarefas_Trabalho_Final,
                  Variaveis_Dominio_Trabalho,
                  Precedencias_Trabalho,
                  Recursos_Aux),
    parse_trabalhos(Rest,
                    Lista_Tarefas,
                    Lista_Variaveis_Dominio,
                    Lista_Precedencias,
                    Recursos_Aux).


% get_all_resources_from_tasks(Tasks, Resources)
get_all_resources_from_tasks([], []).
get_all_resources_from_tasks([task(_, _, _, Resources, _)|Rest_Tasks], [Resources|Rest_Resources]):-
  get_all_resources_from_tasks(Rest_Tasks, Rest_Resources).


% parse_tarefas_into_multi(Lista_Tasks, Lista_Tasks_Multi)
parse_tarefas_into_multi([],[]).
parse_tarefas_into_multi([task(S,D,E,Rec-_Oper,Ti)|Rest_Task], [task(S,D,E,Rec,Ti)|Rest_Multi]):-
  parse_tarefas_into_multi(Rest_Task, Rest_Multi).


% parse_tarefas_into_operators(Lista_Tasks, Lista_Tasks_Oper)
parse_tarefas_into_operators([],[]).
parse_tarefas_into_operators([task(S,D,E,_Rec-Oper,Ti)|Rest_Task], [task(S,D,E,Oper,Ti)|Rest_Multi]):-
  parse_tarefas_into_operators(Rest_Task, Rest_Multi).


%  parse_maquinas_into_cumulatives(Input_Recursos,Output_Maquinas_Parsed,Num),
parse_operators_into_cumulatives([],[],_).
parse_operators_into_cumulatives([Limit_Op|Rest_Operators],
                                [machine(Num,Limit_Op)|Rest_Parsed_Opers],Num):-
    Num2 is Num + 1,
    parse_operators_into_cumulatives(Rest_Operators, Rest_Parsed_Opers, Num2).


% parse_tarefas_into_duplicates(Output_Tarefas_TEMP,Output_Tarefas_Final)
parse_tarefas_into_duplicates([], []).
parse_tarefas_into_duplicates([task(S,D,E,Operators,_TID)|Rest_Task],
                              [Current_Set|Rest_Dups]):-
    duplicate_task(S,D,E,Operators,1,Current_Set),
    parse_tarefas_into_duplicates(Rest_Task, Rest_Dups).


% duplicate_task(Start, Dur, End, Lista_Operators, NumMaquina, Result)
duplicate_task(_,_,_,[],_,[]).
duplicate_task(S,D,E,[Operator|Rest_Operators], Num_Mach, [task(S,D,E,Operator,Num_Mach)|Rest_Res]):-
  Num_Mach2 is Num_Mach + 1,
  duplicate_task(S,D,E,Rest_Operators, Num_Mach2, Rest_Res).
%---------------------------------------------------------------------------------------------------


%--------------------------------MAIN---------------------------------------------------------------
mp(Input_Trabalhos, Input_Recursos, Input_Operadores):-
  % SHOW INPUT TO THE PROBLEM
  write('Input trabalhos:'), nl, write(Input_Trabalhos), nl, nl,
  write('Input recursos:'), nl, write(Input_Recursos), nl, nl,
  write('Input operadores:'), nl, write(Input_Operadores), nl, nl,

  reset_timer,
  % PARSE AND SHOW OUTPUT TO USE
  parse_recursos_operador(Input_Operadores, Output_Operadores_Limites),
  write('Output operadores:'), nl, write(Output_Operadores_Limites), nl, nl,
  parse_recursos_maquina(Input_Recursos, Output_Recursos_Limites, Recursos_Aux),
  write('Output recursos:'), nl, write(Output_Recursos_Limites), nl, nl,
  write('Output recursos aux:'), nl, write(Recursos_Aux), nl, nl,
  parse_trabalhos(Input_Trabalhos,
                  Output_Tarefas,
                  Output_Variaveis_Dominio,
                  Output_Precedencias,
                  Recursos_Aux),

  append(Output_Variaveis_Dominio, Output_Variaveis_Dominio_Final), % flattend this list
  get_start_end(Output_Variaveis_Dominio_Final, Output_Start_Vars, Output_End_Vars),
  append(Output_Tarefas, Output_Tarefas_Flat), % flattend this list

  write('Output tarefas geradas:'), nl, write(Output_Tarefas_Flat), nl,
  write('Output variaveis dominio:'), nl, write(Output_Variaveis_Dominio), nl,
  write('Output inicios:'), nl, write(Output_Start_Vars), nl,
  write('Output fim:'), nl, write(Output_End_Vars), nl,

  append(Output_Precedencias, Output_Precedencias_Flat),
  append(Output_Precedencias_Flat, Output_Precedencias_Flat_Flat),
  write('Output precedencias:'), nl, write(Output_Precedencias_Flat_Flat), nl,
  parse_duracoes(Output_Tarefas_Flat, Output_Duracoes),
  write('Output duracoes:'), nl, write(Output_Duracoes), nl,

  % restrições dos tempos de inicio e fim
  sum(Output_Duracoes, #=, Total_Durations),
  write('Output total duracao:'), nl, write(Total_Durations), nl,

  minimum(Min_Duration, Output_Duracoes),
  Max_Start #= Total_Durations - Min_Duration,
  % definicao dominios
  domain(Output_Start_Vars, 0, Max_Start),
  domain(Output_End_Vars, Min_Duration, Total_Durations),

  % restricoes com multi_cumulative
  append(Output_Recursos_Limites, Output_Operadores_Limites, Output_Recursos_Final),
  write('Output total recursos limite:'), nl, write(Output_Recursos_Final), nl,

  %-----------------------------------TEST FOR CUMULATIVES------------------------------------
  % domain([S,E,S1,E1,M,M1],0,20),
  % cumulatives([task(S,3,E,5,M),task(S1,4,E1,5,M1)],[machine(1,9),machine(2,9)],[bound(upper)]),
  % labeling([],[S,E,S1,E1,M,M1])
  %-------------------------------------------------------------------------------------------------

  % %-----------------------------------OLD MODULE------------------------------------------------
  % get_all_resources_from_tasks(Output_Tarefas_Flat, Tasks_Resources),
  % write('R_antes:'), nl, write(Tasks_Resources), nl,
  % append(Tasks_Resources, Tasks_Resources_Flat),
  % % FUNCION MINIMIZE DIFFERENCE SQUARES COOL
  % labeling([down],Tasks_Resources_Flat), % mudar o down pa ver as merdas a falecer
  % write('R_depois:'), nl, write(Tasks_Resources), nl,
  % %-------------------------------------------------------------------------------------------------

  %-------------------------------------CUMULATIVE WITH OPERATORS-----------------------------------
  parse_tarefas_into_operators(Output_Tarefas_Flat, Output_Tarefas_TEMP),
  parse_tarefas_into_duplicates(Output_Tarefas_TEMP,Output_Tarefas_Final),
  append(Output_Tarefas_Final,Output_Tarefas_Final_Flat),
  parse_operators_into_cumulatives(Input_Operadores,Output_Operators_Parsed,1),
  cumulatives(Output_Tarefas_Final_Flat,Output_Operators_Parsed,[bound(upper)]),
  %-------------------------------------------------------------------------------------------------

  %-------------------------------------MULTI_CUMULATIVE WITH RESOURCES-----------------------------
  parse_tarefas_into_multi(Output_Tarefas_Flat, Output_Tarefas_Multi),
  multi_cumulative(Output_Tarefas_Multi,
                   Output_Recursos_Limites,
                   [precedences(Output_Precedencias_Flat_Flat)]),
  %-------------------------------------------------------------------------------------------------

  % tempo em que terminou a ultima tarefa
  maximum(Max_End, Output_End_Vars),

  % juncao listas
  append(Output_Start_Vars, Output_End_Vars, Lista_Tempos_Final),

  write('Tempo de preparacao:'), nl, print_time, nl,
  reset_timer,

  % IR BUSCAR AS OUTRAS VARIAVEIS DE DOMINIO PARA LABELING
  labeling([minimize(Max_End), bisect, ffc, time_out(10000, _)], Lista_Tempos_Final),

  write('Tempo resolucao:'), nl, print_time, nl, nl,

  output_Result(Output_Tarefas_Flat),

  write('Max possible size is - '), write(Total_Durations), nl,
  write('Full process ends at - '), write(Max_End), nl, nl,
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

% create_list(+Size, +Value, -List).
create_list(0, _, []).
create_list(Number, Value, [Value|Result2]):-
  N2 is Number-1,
  N2 @>= 0,
  create_list(N2, Value, Result2).
%---------------------------------------------------------------------------------------------------
