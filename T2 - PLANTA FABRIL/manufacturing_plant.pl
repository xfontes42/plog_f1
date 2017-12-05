:-use_module(library(clpfd)).
:-use_module(library(lists)).

% Lista_Recursos -> LISTA DE RECURSOS [500, 400, ...] , 500 TIPO A, 400 TIPO B, ETC
% Lista_Trabalhos -> LISTA DE TRABALHOS [W1, W2]
% Trabalho(id_trabalho, LISTA TAREFAS) -> LISTA TAREFAS [ T1, T2, ...]
% Tarefa(local_id, duracao, [consomeA, consomeB, ...], [precisa_local_idX, precisa_local_idY, ...])

lista_trabalhos_1 :- fail.
lista_recursos_1  :- fail.

lista_trabalhos_10 :- fail.
lista_recursos_10  :- fail.

lista_trabalhos_100 :- fail.
lista_recursos_100  :- fail.

lista_trabalhos_1000 :- fail.
lista_recursos_1000  :- fail.

% TODO ver página 475 Manual Sicstus -> multi_cumulative

parse_precedencias(Id_Trabalho, Lista_Precedencias, Listas_Precedencias_Final):- fail.
  % usar forma que fala no sicstus

parse_lista_tarefas(Id_Trabalho, Lista_Tarefas, Lista_Tarefas_Final):- fail.
  % parse das tarefas em tasks,
  % set id correspondente ao trabalhos
  % parse_precedencias
  % colocar precedencias

parse_lista_trabalhos(Lista_Trabalhos, Lista_Tarefas):- fail.
  % vai usar parse tarefas
  % percorre TRABALHOS
  % chama parse_lista_tarefas
  % da append na Lista_Tarefas

gerador_de_problema(Lista_Trabalhos, Lista_Recursos, Escala_Problema):- fail.
  % gerar problemas usando o random

manufacture_phase_matrix(Lista_Trabalhos, Lista_Recursos):- fail
  %> definir variaveis
    % parse da lista de trabalhos em tarefas
    parse_lista_trabalhos(Lista_Trabalhos, Lista_Tarefas),
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
