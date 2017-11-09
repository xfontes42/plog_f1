:-dynamic game_mode/1.
:-dynamic game_difficulty/1.
:-dynamic board_size/1.
:-dynamic current_player/1.
:-dynamic user_play_as/1.
:-dynamic max_points/1.

game_mode(1).
game_difficulty(1).
board_size(9).
current_player(black).
user_play_as(white).
max_points(0).

% set_max_points(+New_Max)
set_max_points(New_Max):-
  nonvar(New_Max),
  integer(New_Max),
  retract(max_points(_)),
  asserta(max_points(New_Max)).

% update_max_points(+Current_Number)
update_max_points(Current_Number):-
  max_points(Current_Max_Points),
  it(
  (Current_Max_Points @< Current_Number),
  (set_max_points(Current_Number))
  ).

% set_game_difficulty(+Difficulty)
set_game_difficulty(Difficulty):-
  nonvar(Difficulty),
  integer(Difficulty),
  retract(game_difficulty(_)),
  asserta(game_difficulty(Difficulty)).

% set_game_mode(+Game_Mode)
set_game_mode(Game_Mode):-
  nonvar(Game_Mode),
  integer(Game_Mode),
  retract(game_mode(_)),
  asserta(game_mode(Game_Mode)).

% set_board_size(+Size)
set_board_size(Size):-
  nonvar(Size),
  integer(Size),
  retract(board_size(_)),
  asserta(board_size(Size)).

% switch_player
switch_player:-
  ite((current_player(black)),
      (retract(current_player(_)), assert(current_player(white))),
      (retract(current_player(_)), assert(current_player(black)))).

% reset_first_player
reset_first_player:-
  retract(current_player(_)),
  assert(current_player(black)).

% clr
clr:-
  write('\33\[2J').

% load
load:-
  consult('C:/Projects/plog_f1/T1 - CONSTA/main.pl').

reload:-
  reconsult('C:/Projects/plog_f1/T1 - CONSTA/main.pl').
