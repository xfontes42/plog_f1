:-dynamic game_mode/1.
:-dynamic game_difficulty/1.
:-dynamic board_size/1.

game_mode(1).
game_difficulty(1).
board_size(9).

set_game_difficulty(Difficulty) :-
  nonvar(Difficulty),
  integer(Difficulty),
  retract(game_difficulty(_)),
  asserta(game_difficulty(Difficulty)).

set_game_mode(Game_Mode) :-
  nonvar(Game_Mode),
  integer(Game_Mode),
  retract(game_mode(_)),
  asserta(game_mode(Game_Mode)).

set_board_size(Size) :-
  nonvar(Size),
  integer(Size),
  retract(board_size(_)),
  asserta(board_size(Size)).
