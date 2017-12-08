:-use_module(library(clpfd)).
:-use_module(library(lists)).

% Lista_Recursos -> LISTA DE RECURSOS [500, 400, ...] , 500 TIPO A, 400 TIPO B, ETC
% Lista_Trabalhos -> LISTA DE TRABALHOS [W1, W2]
% Trabalho(id_trabalho, LISTA TAREFAS) -> LISTA TAREFAS [ T1, T2, ...]
% Tarefa(local_id, duracao, [consomeA, consomeB, ...], [precisa_local_idX, precisa_local_idY, ...])

lista_trabalhos_1([trabalho(1,[tarefa(1, 3, [4,8], []),
                               tarefa(2, 4, [10,20], [1]),
                               tarefa(3, 5, [5,5], [1])]),
                   trabalho(2,[%tarefa(1, 3, [4,8], []),
                               %tarefa(2, 4, [10,20], [1]),
                               tarefa(2, 2, [5,5], [])])
                               ]).
lista_recursos_1([20,30]).

lista_trabalhos_10 :- fail.
lista_recursos_10  :- fail.

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

gerador_de_problema(_Lista_Trabalhos, _Lista_Recursos, _Escala_Problema):- fail.
  % gerar problemas usando o random

% TODO: check this out -> lista_trabalhos_1(X), lista_recursos_1(Y), manufacture_phase_matrix(X,Y).

manufacture_phase_matrix(_Lista_Trabalhos, _Lista_Recursos):-
  %> definir variaveis
    % parse da lista de trabalhos em tarefas
    write('here1'), nl,
    parse_lista_trabalhos(_Lista_Trabalhos, Lista_Tarefas, Lista_Variaveis_Dominio, Lista_Precedencias),
    write('here2'), nl,
    append(Lista_Variaveis_Dominio, Lista_Variaveis_Dominio_Flat), % flattend this list
    get_start_end(Lista_Variaveis_Dominio_Flat, Start_Vars, End_Vars),
    % domain(Lista_Variaveis_Dominio_Flat, 0, sup), nl,
    write('here3'), nl,
    append(Lista_Tarefas, Lista_Tarefas_Flat),
    write(Lista_Tarefas_Flat), nl,
    write('here4'), nl,
    write(Start_Vars), nl,
    write(End_Vars), nl,
    write('here5'), nl,
    append(Lista_Precedencias, Lista_Precedencias_Flat),
    append(Lista_Precedencias_Flat, Lista_Precedencias_Flat_Flat),
    write(Lista_Precedencias_Flat_Flat), nl,
    write('here6'), nl,
    parse_lista_recursos(_Lista_Recursos,Lista_Recursos_Final),
    write(Lista_Recursos_Final), nl,


    parse_duracoes(Lista_Tarefas_Flat, Duracoes),
    write(Duracoes), nl,
    sum(Duracoes, #=, Total_D),
    minimum(Min_D, Duracoes),
    Max_Start #= Total_D - Min_D,
    % definicao dominio
    domain(Start_Vars, 0, Max_Start),
    domain(End_Vars, Min_D, Total_D),

    % definicao restricoes
    % adicionar restriçoes dos dominios com "sum_list"
    multi_cumulative(Lista_Tarefas_Flat, Lista_Recursos_Final, [precedences(Lista_Precedencias_Flat_Flat)]),

    % labelling
    append(Start_Vars, End_Vars, Final),
    labeling([], Final),
    write('here8'), nl,
    write(Start_Vars), nl,
    write(End_Vars), nl
    .


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
