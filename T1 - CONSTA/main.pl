:-include('menus.pl').
:-include('utilities.pl').
:-include('draw_board.pl').
:-include('user_inputs.pl').
:-include('board.pl').

play_consta :- repeat, nl, presentation,
              write('Choose: '),
              once(read(X)),
              integer(X), X@>0, X@<6,
              nl, write('Good Choice').


menu(Mode) :-
  presentation,
  select_game_mode(Mode).


start_game(Board) :-
  seleciona_tamanho_tab(_Tamanho), %DONE
  create_matrix(_Tamanho,empty,Board). %DONE
  %printBoard(Board). %DONE

teste :-
  seleciona_tamanho_tab(_Tamanho),
  create_matrix(_Tamanho,empty,_X),
  printBoard(_X).

teste2 :-
  start_game(Board),
  printBoard(Board).

% teste3 :-
%   repeat,
%   (menu(Mode);
%   write('começou o jogo.'),fail).

consta_game :-
  repeat,
  once(menu(Mode)),
  once(it((Mode \== 4,Mode \== 5),(
    start_game(Board),
    printBoard(Board)))),
  once(it(Mode == 4, select_dificulty(Dificulty))),
  once(ite(Mode == 5, true,fail)).
