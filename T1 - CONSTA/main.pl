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

play_piece(Board_In, Board_Out, Piece_To_Play):-
  repeat,
    once(seleciona_local(X,Y)),
    ite((once(valid_move(Board_In,X,Y,Piece_To_Play,NewElement))),
        (once(set_element_at(Board_In, Y, X, NewElement, Board_Out))),
        (write('Jogada inválida.'),nl, fail)).


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

game_cycle(Board, Mode):-
  play_round(Board, New_Board, Mode),
  printBoard(New_Board),
  % verificação se ganhou/perdeu
  switch_player,
  game_cycle(New_Board, Mode).


% game_cycle(Board) :-
%   play(Board,NewBoard),
%   game_cycle(NewBoard).
%
% play(Board,NewBoard) :-
%   repeat,
%   once(current_player(Player)),
%   write('Playing '),write(Player),write(' pieces.'),nl,
%   last_play(LastType),
%   ite(LastType == 0,
%     (seleciona_jogada(Type),
%     seleciona_local(X,Y),
%     getPlay(Type,Player,Value)),
%     (seleciona_local(X,Y),
%     getPlay(Type,Player,Value))),
%   !,
%   ite(valid_move(Board,X,Y,Value,NewElement),
%     (set_element_at(Board, Y, X, NewElement, NewBoard),
%     ite(Type==52,
%       switch_player,
%       switch_LastType),
%     printBoard(NewBoard)),
%     ( get_element_at(Board,X,Y,NewElement),
%       set_element_at(Board, Y, X, NewElement, NewBoard),
%       write('Jogada inválida.'),nl)).

consta_game :-
  repeat,
  once(menu(Mode)),
  once(it((Mode \== 4,Mode \== 5),(
    start_game(Board),
    game_cycle(Board, Mode)))),
  once(it(Mode == 4, select_dificulty(_Dificulty))),
  once(ite(Mode == 5, true,fail)).
