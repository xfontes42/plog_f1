:-use_module(library(clpfd)).
:-use_module(library(lists)).

% Lista_Recursos -> LISTA DE RECURSOS [500, 400, ...] , 500 TIPO A, 400 TIPO B, ETC
% Lista_Trabalhos -> LISTA DE TRABALHOS [W1, W2]
% Trabalho(id_trabalho, LISTA TAREFAS) -> LISTA TAREFAS [ T1, T2, ...]
% Tarefa(local_id, duracao, [consomeA, consomeB, ...], [precisa_local_idX, precisa_local_idY, ...])

lista_trabalhos_1([trabalho(1,[tarefa(1, 3, [4,8], [])])]).
lista_recursos_1([10,20]).

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


parse_precedencias(_, [], []).
parse_precedencias(ID_Trabalho, [H|Tail], [H2, Tail2]):-
  H2 #= ID_Trabalho * 1000 + H,
  parse_precedencias(ID_Trabalho, Tail, Tail2).
  % usar forma que fala no sicstus


parse_lista_tarefas(ID, [], [], [], []).
parse_lista_tarefas(ID, [tarefa(ID_Tarefa, Duracao, Recursos, Precedencias)|Rest],
                        [task(Oi, Duracao, Ei, Recursos, Ti)|Rest_Task],
                        [Oi,Ei|Variaveis_Dominio_Trabalho],
                        [Precedencias_Final|Precedencias_Trabalho]):-
   Ti #= ID * 1000 + ID_Tarefa,
   parse_precedencias(ID, Precedencias, Precedencias_Final).
  % parse das tarefas em tasks,
  % set id correspondente ao trabalhos
  % parse_precedencias
  % colocar precedencias
parse_lista_trabalhos([], [], [], []).
parse_lista_trabalhos([trabalho(ID,Tarefas_Trabalho)|Rest],
                      [Tarefas_Trabalho_Final|Lista_Tarefas],
                      [Variaveis_Dominio_Trabalho|Lista_Variaveis_Dominio],
                      [Precedencias_Trabalho|Lista_Precedencias]):-
  write('antes_parse_tarefas'), nl,
  parse_lista_tarefas(ID, Tarefas_Trabalho, Tarefas_Trabalho_Final, Variaveis_Dominio_Trabalho, Precedencias_Trabalho),
  write('depois_parse_tarefas'), nl,
  parse_lista_trabalhos(Rest, Lista_Tarefas, Lista_Variaveis_Dominio, Lista_Precedencias).
  % vai usar parse tarefas
  % percorre TRABALHOS
  % chama parse_lista_tarefas
  % da append na Lista_Tarefas

gerador_de_problema(Lista_Trabalhos, Lista_Recursos, Escala_Problema):- fail.
  % gerar problemas usando o random


% TODO: check this out -> lista_trabalhos_1(X), lista_recursos_1(Y), manufacture_phase_matrix(X,Y).

manufacture_phase_matrix(Lista_Trabalhos, Lista_Recursos):-
  %> definir variaveis
    % parse da lista de trabalhos em tarefas
    write('here1'), nl,
    parse_lista_trabalhos(Lista_Trabalhos, Lista_Tarefas, Lista_Variaveis_Dominio, Lista_Precedencias),
    write('here2'), nl,
    append(Lista_Variaveis_Dominio, Lista_Vars),
    domain(Lista_Vars, 0, sup),
    write('here3'), nl,
    write(Lista_Tarefas)
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
  .
