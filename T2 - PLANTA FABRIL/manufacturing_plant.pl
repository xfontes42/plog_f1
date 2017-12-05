:-use_module(library(clpfd)).
:-use_module(library(lists)).

% Lista_Recursos -> LISTA DE RECURSOS [500, 400, ...] , 500 TIPO A, 400 TIPO B, ETC
% Lista_Trabalhos -> LISTA DE TRABALHOS [W1, W2]
% Trabalho -> LISTA TAREFAS [ T1, T2, ...]
% Tarefa(local_id, duracao, [consomeA, consomeB, ...], [precisa_local_idX, precisa_local_idY, ...])

lista_trabalhos_1 :- fail.
lista_recursos_1  :- fail.

lista_trabalhos_10 :- fail.
lista_recursos_10  :- fail.

lista_trabalhos_100 :- fail.
lista_recursos_100  :- fail.

lista_trabalhos_1000 :- fail.
lista_recursos_1000  :- fail.

gerador_de_problema(Lista_Trabalhos, Lista_Recursos, Escala_Problema):- fail.

manufacture_phase_matrix(Lista_Trabalhos, Lista_Recursos):- fail
  % minimizar o maximo de tempo final
  .
