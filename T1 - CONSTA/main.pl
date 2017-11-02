:-use_module(library(system)).
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

% play_human_piece(+Board_In, -Board_Out, +Piece_To_Play)
play_human_piece(Board_In, Board_Out, Piece_To_Play):-
  repeat,
    once(seleciona_local(X,Y)),
    ite((once(valid_move(Board_In,X,Y,Piece_To_Play,NewElement))),
        (once(set_element_at(Board_In, X, Y, NewElement, Board_Out))),
        (write('Jogada inválida.'),nl, fail)).

% play_computer_piece(+Board_In, -Board_Out, +Piece_S, +Piece_D, +Difficulty)
play_computer_piece(Board_In, Board_Out, Piece_S, Piece_D, _Difficulty):-
  setof(X_S-Y_S-Element_S, valid_move(Board_In, X_S, Y_S, Piece_S, Element_S), List_Singles),
  setof(X_D-Y_D-New_Element_D, valid_move(Board_In, X_D, Y_D, Piece_D, New_Element_D), List_Doubles),
  length(List_Singles, Size_Singles),
  length(List_Doubles, Size_Doubles),
  random(0,Size_Doubles,Index_Doubles), !,
  ite(
  (Size_Singles @< 2),
  (nth0(Index_Doubles, List_Doubles, X_Play-Y_Play-New_Element_Play),
    set_element_at(Board_In, X_Play, Y_Play, New_Element_Play, Board_Out)),
  (random(0,2,Choose_One),
    ite((Choose_One == 0),
        ((generate_random(0,Size_Singles,Index_Singles,Index_Singles_2),
          nth0(Index_Singles, List_Singles, X_Play-Y_Play-New_Element_Play),
          set_element_at(Board_In, X_Play, Y_Play, New_Element_Play, Board_Temp),
          nth0(Index_Singles_2, List_Singles, X_Play_2-Y_Play_2-New_Element_Play_2),
          set_element_at(Board_Temp,X_Play_2, Y_Play_2, New_Element_Play_2, Board_Out))),
        (nth0(Index_Doubles, List_Doubles, X_Play-Y_Play-New_Element_Play),
          set_element_at(Board_In, X_Play, Y_Play, New_Element_Play, Board_Out))))
  ).
  %sleep(0.2).
  % get_code(_).
  % duplicate(Board_In, Board_Out).

% play_computer(+Board_In, -Board_Out, +Player)
play_computer(Board_In, Board_Out,Player):-
  game_difficulty(Difficulty),
  ite(
  (Player == black),
  (play_computer_piece(Board_In, Board_Out, black, black2, Difficulty)),
  (play_computer_piece(Board_In, Board_Out, white, white2, Difficulty))).

% play_round(+Board_In, -Board_Out, +_Mode)
play_round(Board_In, Board_Out, Mode):-
    once(current_player(Player)),
    write('Playing '),write(Player),write(' pieces.'),nl,
    once(user_play_as(User_Player)),
    ite(
    ((Mode == 3, Player \== User_Player);(Mode==2)),
    (play_computer(Board_In, Board_Out,Player)),
    (seleciona_jogada(Type),
      getPlay(Type, Player, Piece_To_Play),
      ite((Type == 52), % d = 52, s = 67
        (valid_move(Board_In, _, _, Piece_To_Play, _N1), !,
          play_human_piece(Board_In, Board_Out, Piece_To_Play)),

        (valid_move(Board_In, _, _, Piece_To_Play, _N2), !,
          play_human_piece(Board_In, Board_Temp, Piece_To_Play),
          valid_move(Board_Temp, _, _, Piece_To_Play, _N3), !,
          play_human_piece(Board_Temp, Board_Out, Piece_To_Play)))
    )).

% check_win(+Board)
check_win(Board):-
  current_player(Player),
  ite(
  (Player == black),
  (check_win_white(Board), write('Winner is WHITE!'), nl, get_code(_), credits, get_code(_)),
  (check_win_black(Board), write('Winner is BLACK!'), nl, get_code(_), credits, get_code(_))).

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
