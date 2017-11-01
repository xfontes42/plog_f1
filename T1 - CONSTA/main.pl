:-include('utilities.pl').
:-include('menus.pl').
:-include('draw_board.pl').
:-include('user_inputs.pl').
:-include('board.pl').

% menu(-Mode)
menu(Mode) :-
  presentation,
  select_game_mode(Mode).

% start_game(-Board)
start_game(Board) :-
  seleciona_tamanho_tab(_Tamanho),
  set_board_size(_Tamanho),
  create_matrix(_Tamanho,empty,Board),
  printBoard(Board).

% play_piece(+Board_In, -Board_Out, +Piece_To_Play)
play_piece(Board_In, Board_Out, Piece_To_Play):-
  repeat,
    once(seleciona_local(X,Y)),
    ite((once(valid_move(Board_In,X,Y,Piece_To_Play,NewElement))),
        (once(set_element_at(Board_In, X, Y, NewElement, Board_Out))),
        (write('Jogada inválida.'),nl, fail)).

% play_round(+Board_In, -Board_Out, +_Mode)
play_round(Board_In, Board_Out, _Mode):-
    once(current_player(Player)),
    write('Playing '),write(Player),write(' pieces.'),nl,
    seleciona_jogada(Type),
    getPlay(Type, Player, Piece_To_Play),
    ite((Type == 52), % d = 52, s = 67
    (valid_move(Board_In, _, _, Piece_To_Play, _N1), !,
      play_piece(Board_In, Board_Out, Piece_To_Play)),

    (valid_move(Board_In, _, _, Piece_To_Play, _N2), !,
      play_piece(Board_In, Board_Temp, Piece_To_Play),
     valid_move(Board_Temp, _, _, Piece_To_Play, _N3), !,
      play_piece(Board_Temp, Board_Out, Piece_To_Play))).


check_win(Matrix):-
  current_player(Player),
  ite(
  (Player == black),
  (check_win_white(Matrix), write("Winner is WHITE!"), nl),
  (check_win_black(Matrix), write("Winner is BLACK!"), nl)).

% game_cycle(+Board, +Mode)
game_cycle(Board, Mode):-
  play_round(Board, New_Board, Mode),
  printBoard(New_Board),
  ite(
  (check_win(New_Board)),
  (true),
  (switch_player,
   game_cycle(New_Board, Mode)
  )).

% consta_game
consta_game :-
  repeat,
  once(menu(Mode)),
  once(it((Mode \== 4,Mode \== 5),(
    start_game(Board),
    game_cycle(Board, Mode)))),
  once(it(Mode == 4, select_dificulty(_Dificulty))),
  once(ite(Mode == 5, true,fail)).
