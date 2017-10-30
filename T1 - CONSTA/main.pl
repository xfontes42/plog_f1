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
  seleciona_tamanho_tab(_Tamanho),
  set_board_size(_Tamanho),
  create_matrix(_Tamanho,empty,Board),
  printBoard(Board).

game_cycle(Board) :-
  play(Board,NewBoard),
  game_cycle(NewBoard).

play(Board,NewBoard) :-
  repeat,
  once(current_player(Player)),
  write('Playing '),write(Player),write(' pieces.'),nl,
  seleciona_jogada(Type),
  seleciona_local(X,Y),
  getPlay(Type,Player,Value),
  !,
  ite(valid_move(Board,X,Y,Value,NewElement),
  (set_element_at(Board, Y, X, NewElement, NewBoard),switch_player,printBoard(NewBoard)),
  ( get_element_at(Board,X,Y,NewElement),
    set_element_at(Board, Y, X, NewElement, NewBoard),
    write('Jogada inválida.'),nl)).

consta_game :-
  repeat,
  once(menu(Mode)),
  once(it((Mode \== 4,Mode \== 5),(
    start_game(Board),
    game_cycle(Board)))),
  once(it(Mode == 4, select_dificulty(_Dificulty))),
  once(ite(Mode == 5, true,fail)).
