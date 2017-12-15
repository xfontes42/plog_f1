:-use_module(library(clpfd)).
:-use_module(library(timeout)).
:-use_module(library(lists)).
:-use_module(library(random)).


% Lista_Recursos -> LISTA DE RECURSOS [500, 400, ...] , 500 TIPO A, 400 TIPO B, ETC
% Lista_Trabalhos -> LISTA DE TRABALHOS [W1, W2]
% Trabalho(id_trabalho, LISTA TAREFAS) -> LISTA TAREFAS [ T1, T2, ...]
% Tarefa(local_id, duracao, [consomeA, consomeB, ...], [precisa_local_idX, precisa_local_idY, ...])


lista_trab_1(
  [trabalho(1,[tarefa(1, 3, [4,8], []),
               tarefa(2, 4, [10,20], [1]),
               tarefa(3, 5, [5,5], [])])]
).
lista_rec_1(
[
  maquinas([10,2,20],[1,0,0]),
  maquinas([1,20,10],[2,0,0]),
  operadores([10,5,4])
]
).

lista_trabalhos_1([trabalho(1,[tarefa(1, 3, [4,8], []),
                               tarefa(2, 4, [10,20], [1]),
                               tarefa(3, 5, [5,5], [])]),
                   trabalho(2,[tarefa(1, 3, [4,7], []),
                               tarefa(2, 4, [10,20], [1]),
                               tarefa(3, 2, [5,5], [1])])
                               ]).
lista_recursos_1([20,30]).

lista_trabalhos_10([trabalho(2,[tarefa(5,3,[3,5],[]),
                                tarefa(4,3,[1,43],[]),
                                tarefa(3,3,[6,19],[]),
                                tarefa(2,3,[6,37],[]),
                                tarefa(1,3,[7,3],[])]),
                    trabalho(1,[tarefa(8,3,[7,34],[]),
                                tarefa(7,3,[2,44],[]),
                                tarefa(6,3,[1,1],[]),
                                tarefa(5,3,[9,3],[]),
                                tarefa(4,3,[8,7],[]),
                                tarefa(3,3,[1,13],[]),
                                tarefa(2,3,[4,36],[]),
                                tarefa(1,3,[4,48],[])])]).
lista_recursos_10([10,50]).

lista_trabalhos_100 :- fail.
lista_recursos_100  :- fail.

lista_trabalhos_1000 :- fail.
lista_recursos_1000  :- fail.

% TODO ver página 475 Manual Sicstus -> multi_cumulative
% task(Oi,Di,Ei,Hsi,Ti) where Oi is the start
% time, Di the non-negative duration, Ei the end time, Hsi the list of non-negative
% resource uses or colors, and Ti the task identifier. The start and end times
% should be domain variables with bounded domains. The other fields should be
% integers.

% precedences(Ps)
% Ps encodes a set of precedence constraints to apply to the tasks.
% Ps should be a list of pairs Ti-Tj where Ti and Tj should be task
% identifiers, denoting that task Ti must complete before task Tj can
% start.

output_Result([]):-
  nl,write('End of Tasks'),nl.
output_Result([Head|Lista_Tarefas]) :-
  output_Task(Head),
  output_Result(Lista_Tarefas).

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

parse_duracoes([], []).
parse_duracoes([task(_,Dur,_,_,_)|Tail], [Dur|Tail2]):-
  parse_duracoes(Tail, Tail2).


parse_lista_recursos([], []).
parse_lista_recursos([H|T], [cumulative(H)|T2]):-
  parse_lista_recursos(T, T2).

get_start_end([], [], []).
get_start_end([H1-H2|Tail], [H1|Tail2], [H2|Tail3]):-
  get_start_end(Tail, Tail2, Tail3).

parse_precedencias(_, _, [], []).
parse_precedencias(ID_Trabalho, Ti ,[H|Tail], [H2-Ti |Tail2]):-
  H2 #= ID_Trabalho * 1000 + H,
  parse_precedencias(ID_Trabalho, Ti,Tail, Tail2).
  % usar forma que fala no sicstus


parse_lista_tarefas(_, [], [], [], []).
parse_lista_tarefas(ID, [tarefa(ID_Tarefa, Duracao, Recursos, Precedencias)|Rest],
                        [task(Oi, Duracao, Ei, Recursos, Ti)|Rest_Task],
                        [Oi-Ei|Variaveis_Dominio_Trabalho],
                        [Precedencias_Final|Precedencias_Trabalho]):-
   Ti #= ID * 1000 + ID_Tarefa,
   parse_precedencias(ID, Ti, Precedencias, Precedencias_Final),
   parse_lista_tarefas(ID, Rest, Rest_Task, Variaveis_Dominio_Trabalho, Precedencias_Trabalho ).
  % parse das tarefas em tasks,
  % set id correspondente ao trabalhos
  % parse_precedencias
  % colocar precedencias
parse_lista_trabalhos([], [], [], []).
parse_lista_trabalhos([trabalho(ID,Tarefas_Trabalho)|Rest],
                      [Tarefas_Trabalho_Final|Lista_Tarefas],
                      [Variaveis_Dominio_Trabalho|Lista_Variaveis_Dominio],
                      [Precedencias_Trabalho|Lista_Precedencias]):-
  % write('antes_parse_tarefas'), nl,
  parse_lista_tarefas(ID, Tarefas_Trabalho, Tarefas_Trabalho_Final, Variaveis_Dominio_Trabalho, Precedencias_Trabalho),
  % write('depois_parse_tarefas'), nl,
  parse_lista_trabalhos(Rest, Lista_Tarefas, Lista_Variaveis_Dominio, Lista_Precedencias).
  % vai usar parse tarefas
  % percorre TRABALHOS
  % chama parse_lista_tarefas
  % da append na Lista_Tarefas


gera_recursos([], []).
gera_recursos([H1|T1], [H2|T2]):-
  random(1,H2,H1),
  gera_recursos(T1, T2).


% gera_precedencias(_, []).

gera_precedencias(1, []).
gera_precedencias(ID, [H1]):-
  random(1,ID,H1).

gera_tarefa(tarefa(ID_Tarefa, 3, Recursos_T, Precedencias), ID_Tarefa, Recursos):-
  gera_recursos(Recursos_T, Recursos),
  gera_precedencias(ID_Tarefa, Precedencias).

gera_trabalho(ID_trabalho, trabalho(ID_trabalho, []), 0, _Recursos).
gera_trabalho(ID_trabalho, trabalho(ID_trabalho, [Tarefa|Resto]), NTarefas, Recursos):-
  gera_tarefa(Tarefa, NTarefas, Recursos),
  NTarefas2 is NTarefas -1,
  gera_trabalho(ID_trabalho, trabalho(ID_trabalho, Resto), NTarefas2, Recursos).

gerador_de_problema([], 0, _, _).
gerador_de_problema([Trabalho|Resto_Trabalhos], NTrabalhos, NTarefas, Recursos):-
  NTarefas_Min is div(NTarefas,10) + 1,
  random(NTarefas_Min, NTarefas, NTarefas2),
  gera_trabalho(NTrabalhos, Trabalho, NTarefas2, Recursos),
  NTrabalhos2 is NTrabalhos - 1,
  gerador_de_problema(Resto_Trabalhos, NTrabalhos2, NTarefas, Recursos).
  % gerar problemas usando o random

% TODO: check this out -> lista_trabalhos_1(X), lista_recursos_1(Y), manufacture_phase_matrix(X,Y).

manufacture_phase_matrix(_Lista_Trabalhos, _Lista_Recursos):-
    reset_timer,
    write('Lista fornecida:'), nl,
    write(_Lista_Trabalhos), nl, nl, nl,
    % write('Parsing Trabalhos.'), nl,
    parse_lista_trabalhos(_Lista_Trabalhos, Lista_Tarefas, Lista_Variaveis_Dominio, Lista_Precedencias),

    % flattening lista variaveis de dominio
    append(Lista_Variaveis_Dominio, Lista_Variaveis_Dominio_Final), % flattend this list

    % write('Variaveis dominio, tempos inicio e fim das tarefas.'), nl,
    get_start_end(Lista_Variaveis_Dominio_Final, Start_Vars, End_Vars),

    % flattening lista de tarefas
    append(Lista_Tarefas, Lista_Tarefas_Flat),

    write('Lista de tarefas gerada:'), nl,
    write(Lista_Tarefas_Flat), nl, nl, nl,
    % write('Tempos inicio:'), nl,
    % write(Start_Vars), nl,
    % write('Tempos fim:'), nl,
    % write(End_Vars), nl,

    % flattening lista precedencias
    append(Lista_Precedencias, Lista_Precedencias_Flat),
    append(Lista_Precedencias_Flat, Lista_Precedencias_Flat_Flat),

    write('Lista de precedencias:'), nl,
    write(Lista_Precedencias_Flat_Flat), nl,
    % write('Parsing lista de recursos.'), nl,
    parse_lista_recursos(_Lista_Recursos,Lista_Recursos_Final),
    % write('Lista de recursos:'), nl,
    % write(Lista_Recursos_Final), nl,

    % write('Parsing duracoes.'), nl,
    parse_duracoes(Lista_Tarefas_Flat, Duracoes),
    % write('Duracoes:'), nl,
    % write(Duracoes), nl,

    % restrições dos tempos de inicio e fim
    sum(Duracoes, #=, Total_D),
    % write('Total Duracoes:'), nl,
    % write(Total_D), nl,
    minimum(Min_D, Duracoes),
    Max_Start #= Total_D - Min_D,
    % definicao dominios
    domain(Start_Vars, 0, Max_Start),
    domain(End_Vars, Min_D, Total_D),

    % restricoes com multi_cumulative
    multi_cumulative(Lista_Tarefas_Flat, Lista_Recursos_Final, [precedences(Lista_Precedencias_Flat_Flat)]),

    % tempo em que terminou a ultima tarefa
    maximum(Max_End, End_Vars),

    % juncao listas
    append(Start_Vars, End_Vars, Lista_Tempos_Final),

    write('Time spent preparing problem:'),
    print_time,
    % LABELING MINIMIZANTE
    reset_timer,

    % We have an optimization problem, seeking an assignment that minimizes
    % (maximizes)  the  domain  variable X. The  labeling  should constrain X to
    % become assigned for all assignments of Variables .  It is useful to combine
    % these option with the time_out/2 , best , and all options (see below).  If
    % these options occur more than once, the last occurrence overrides previous
    % ones.

    labeling([minimize(Max_End), bisect, ffc, time_out(5000, _)], Lista_Tempos_Final),

    write('Time spent resolving problem:'),
    print_time,
    % LABELING NORMAL
    % labeling([], Lista_Tempos_Final),

    nl,
    output_Result(Lista_Tarefas_Flat),

    write('Tempos comeco:'), nl,
    write(Start_Vars), nl,
    write('Tempos fim:'), nl,
    write(End_Vars), nl, nl,

    write('Max possible size is - '), write(Total_D), nl,
    write('Full process ends at - '), write(Max_End), nl, nl,
    write('Statistics:'), nl,
  	fd_statistics.

reset_timer :- statistics(walltime,_).
print_time :-
	statistics(walltime,[_,T]),
	TS is ((T//10)*10)/1000,
	nl, write('Time: '), write(TS), write('s'), nl, nl.
    % definir lista de todas as tarefas com tempos para imprimir no fim
    % definir limites como pede multi_cumulative

  %> definir dominios
    % dominio do tempo em inteiros sem limite superior, começar em zero

  %> defenir restriçoes
    % restricoes do multi_cumulative , ver pagina 474 manual sicstus

  %> labeling
    % minimizar o maximo de tempo final
    % exeprimentar varios metodos de procura
    % experimentar definir o proprio metodo de procura


con :- consult('manufacturing_plant.pl').

recon :-  reconsult('manufacturing_plant.pl').

reload :- reconsult('C:/Users/edusw/OneDrive/Documents/GitHub/plog_f1/T2 - PLANTA FABRIL/manufacturing_plant.pl').

clr:- write('\33\[2J').


teste1:- lista_trabalhos_1(X), lista_recursos_1(Y), manufacture_phase_matrix(X,Y).

teste10:- lista_trabalhos_10(X), lista_recursos_10(Y), manufacture_phase_matrix(X,Y).

teste_gerador:- X=[10,50], gerador_de_problema(L, 2, 10, X),  manufacture_phase_matrix(L,X).

teste_gerador10:- X=[16,48,10], gerador_de_problema(L, 3, 15, X),  manufacture_phase_matrix(L,X).
