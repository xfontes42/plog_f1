:-include('menus.pl').
:-include('utilities.pl').
:-include('draw_board.pl').
:-include('user_inputs.pl').

play_consta :- repeat, nl, presentation,
              write('Choose: '),
              once(read(X)),
              integer(X), X@>0, X@<6,
              nl, write('Good Choice').

teste :-
  seleciona_tamanho_tab(_Tamanho),
  create_matrix(_Tamanho,empty,_X),
  printBoard(_X).
