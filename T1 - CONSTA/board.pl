:-include('dynamic_globals.pl').
:-use_module(library(lists)).

% create_list(+Size, +Value, -List).
create_list(0, _, []).
create_list(Number, Value, [Value|Result2]):-
  N2 is Number-1,
  N2 @>= 0,
  create_list(N2, Value, Result2).

% create_matrix(+Size, +Value, -Matrix).
create_matrix(Number, Value, Result):-
  create_list(Number, Value, List),
  create_list(Number, List, Result).

% get_element_at(+Matrix, +X, +Y, -Element)
get_element_at(Matrix, X, Y, Element):-
   nth0(Y,Matrix,Row),
   nth0(X,Row,Element).

% set_element_at(+Matrix, +X, +Y, +Value, -New_Matrix)
set_element_at(Matrix, X, Y, Value, New_Matrix):-
  append(RowPrefix,[Row|RowSufix],Matrix),
  length(RowPrefix,Y) ,
  append(ColPrefix,[_|ColSufix],Row) ,
  length(ColPrefix,X) ,
  append(ColPrefix,[Value|ColSufix],RowNew) ,
  append(RowPrefix,[RowNew|RowSufix],New_Matrix).

% sum_pieces(+Current, +Top, -Result)
sum_pieces(empty, black, black).
sum_pieces(empty, white, white).
sum_pieces(empty, black2, black2).
sum_pieces(empty, white2, white2).
sum_pieces(black, black, black2).
sum_pieces(white, white, white2).

% get_points_from_square(+Elem1, +Elem2, +Elem3, +Elem4, -Points_White, -Points_Black)
get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black):-
  getPoints(Elem1, P1), !, getPoints(Elem2,P2), !, getPoints(Elem3,P3), !, getPoints(Elem4, P4), !,
  Sum is (P1 + P2 + P3 + P4),
  Points_White is mod(Sum,10),
  Points_Black is div(Sum,10).

% check_cross_cut_up_left(+Matrix, +X, +Y, +Value)
check_cross_cut_up_left(Matrix, X, Y, New_Element):-
  X_Across is X-1, Y_Across is Y-1,
  ite(
  (X == 0 ; Y == 0; (
    get_element_at(Matrix, X_Across, Y_Across, Element_Across),
    getPerson(Element_Across, Person1),
    getPerson(New_Element, Person2),
    Person1 \== Person2)
  ),
  (true),
  ( get_element_at(Matrix, X_Across, Y_Across, Elem1),
    get_element_at(Matrix, X, Y_Across, Elem2),
    get_element_at(Matrix, X_Across, Y, Elem3),
    get_points_from_square(Elem1,Elem2,Elem3, New_Element, P_White, P_Black),!,
    P_White \== P_Black)).

% check_cross_cut_up_right(+Matrix, +X, +Y, +Value)
check_cross_cut_up_right(Matrix, X, Y, New_Element):-
  length(Matrix, Size_Matrix),
  Max_Index is Size_Matrix -1,
  X_Across is X+1, Y_Across is Y-1,
  ite(
  (X == Max_Index ; Y == 0; (
    get_element_at(Matrix, X_Across, Y_Across, Element_Across),
    getPerson(Element_Across, Person1),
    getPerson(New_Element, Person2),
    Person1 \== Person2)
  ),
  (true),
  ( get_element_at(Matrix, X_Across, Y_Across, Elem1),
    get_element_at(Matrix, X, Y_Across, Elem2),
    get_element_at(Matrix, X_Across, Y, Elem3),
    get_points_from_square(Elem1,Elem2,Elem3, New_Element, P_White, P_Black),
    P_White \== P_Black)).

% check_cross_cut_down_left(+Matrix, +X, +Y, +Value)
check_cross_cut_down_left(Matrix, X, Y, New_Element):-
  X_Across is X-1, Y_Across is Y+1,
  length(Matrix, Size_Matrix),
  Max_Index is Size_Matrix-1,
  ite(
  (X == 0 ; Y == Max_Index; (
    get_element_at(Matrix, X_Across, Y_Across, Element_Across),
    getPerson(Element_Across, Person1),
    getPerson(New_Element, Person2),
    Person1 \== Person2)
  ),
  (true),
  ( get_element_at(Matrix, X_Across, Y_Across, Elem1),
    get_element_at(Matrix, X, Y_Across, Elem2),
    get_element_at(Matrix, X_Across, Y, Elem3),
    get_points_from_square(Elem1,Elem2,Elem3, New_Element, P_White, P_Black),
    P_White \== P_Black)).

% check_cross_cut_down_right(+Matrix, +X, +Y, +Value)
check_cross_cut_down_right(Matrix, X, Y, New_Element):-
  length(Matrix, Size_Matrix),
  Max_Index is Size_Matrix -1,
  X_Across is X+1, Y_Across is Y+1,
  ite(
  (X == Max_Index ; Y == Max_Index; (
    get_element_at(Matrix, X_Across, Y_Across, Element_Across),
    getPerson(Element_Across, Person1),
    getPerson(New_Element, Person2),
    Person1 \== Person2)
  ),
  (true),
  ( get_element_at(Matrix, X_Across, Y_Across, Elem1),
    get_element_at(Matrix, X, Y_Across, Elem2),
    get_element_at(Matrix, X_Across, Y, Elem3),
    get_points_from_square(Elem1,Elem2,Elem3, New_Element, P_White, P_Black),
    P_White \== P_Black)).

% check_cross_cut(+Matrix, +X, +Y, +Value)
check_cross_cut(Matrix, X, Y, New_Element):-
  check_cross_cut_up_left(Matrix, X, Y, New_Element),!,
  check_cross_cut_up_right(Matrix, X, Y, New_Element),!,
  check_cross_cut_down_left(Matrix, X, Y, New_Element),!,
  check_cross_cut_down_right(Matrix, X, Y, New_Element).

% valid_move(+Matrix, +X, +Y, +Value, -New_Element)
valid_move(Matrix, X, Y, Value, New_Element):-
  get_element_at(Matrix, X, Y, Current_Piece),
  Current_Piece \== black2, Current_Piece \== white2, % Cannot play on top of a double
  current_player(Curr_Player),
  once(logic_or((Curr_Player == Current_Piece, Curr_Player == Value),(Current_Piece == empty))), % Cannot play different colors or a double over a single
  sum_pieces(Current_Piece, Value, New_Element),
  check_cross_cut(Matrix, X, Y, New_Element).






% % search_black(+Matrix, +Matrix_Changing, +X, +Y)
% search_black(_,[],_,_).
% search_black(Matrix, [Current_Line|Rest], X, Y):-
%   Y2 is Y+1,
%
%   ((X2 == X_Diagonal_Left);(X2 == X_Diagona_Right);(X2 == X)),
%   get_element_at(Matrix, X2, Y, Elem1),
%   get_element_at(Matrix, X2, Y2, Elem2),
%   get_element_at(Matrix, X, Y, Elem3),
%   get_element_at(Matrix, X, Y2, Elem4),
%   get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black),
%   Points_White @=< Points_Black,
%   search_black(Matrix, Rest, X2, Y2).
%
% % check_win_black(+Matrix)
% check_win_black([First_Row|Rest]):-
%   logic_or(nth0(X,First_Row,black), nth0(X,First_Row,black2)),
%   Y is 0,
%   search_black([First_Row|Rest],Rest, X, Y).


% get_points_black(+Matrix)
get_points_black(_, [], _, _, _):-
  max_points(X),
  Y is X-1,
  set_max_points(Y).

get_points_black(Matrix, [First_Row|Rest], Current_Number, X, Y):-
  Y2 is Y + 1,
  ite((Current_Number == 0),
  ( % THEN EXTERIOR
    ite(
      (logic_or(nth0(X3,First_Row,black), nth0(X3,First_Row,black2))),
      ( % THEN INTERIOR
        New_Current_Number is Current_Number + 1,
        update_max_points(New_Current_Number),
        get_points_black(Matrix, [First_Row|Rest], New_Current_Number, X3, Y)
      ), % END_THEN INTERIOR
      (
      update_max_points(Current_Number),
      get_points_black(Matrix,Rest,0,0,Y2)
      ) % ELSE INTERIOR
    )
  ), % END_THEN EXTERIOR
  ( % ELSE EXTERIOR
  ite(
    % IF
    (logic_or(nth0(X2, First_Row, black), nth0(X2, First_Row, black2))),
    % THEN
    (
        X_Diagonal_Left is X-1,
        X_Diagona_Right is X+1,

        ite(
          % IF
          ((X2 == X_Diagonal_Left);(X2 == X_Diagona_Right);(X2 == X)),
          % THEN
          (
              get_element_at(Matrix, X2, Y, Elem1),
              get_element_at(Matrix, X2, Y2, Elem2),
              get_element_at(Matrix, X, Y, Elem3),
              get_element_at(Matrix, X, Y2, Elem4),
              get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black),
              ite(
                % IF
                (Points_White @=< Points_Black),
                % THEN
                (
                  New_Current_Number is Current_Number + 1,
                  update_max_points(New_Current_Number),
                  get_points_black(Matrix, Rest, New_Current_Number, X2, Y2)
                ),
                % ELSE
                (
                  update_max_points(Current_Number),
                  get_points_black(Matrix,Rest,0,0,Y2)
                )
              )
          ),
          % ELSE
          (
              update_max_points(Current_Number),
              get_points_black(Matrix,Rest,0,0,Y2)
          )
         )

      ),
      % ELSE
      (
        get_points_black(Matrix,Rest,0,0,Y2)) % ELSE INTERIOR
      )
  )).  % END_ELSE EXTERIOR


% eval_board_black(+Matrix, -Number_Black)
eval_board_black(Matrix, Number_Black):-
  get_points_black(Matrix, Matrix, 0, 0, 0),
  max_points(Number_Black).

% eval_board_white(+Matrix, -Number_White)
eval_board_white(_Matrix, Number_White):-

  max_points(Number_White).

% eval_board(+Matrix, -Number_Black, -Number_White)
eval_board(Matrix, Number_Black, Number_White):-
  set_max_points(0),
  eval_board_black(Matrix, Number_Black),
  set_max_points(0),
  eval_board_white(Matrix, Number_White).
  % limit(Number_Black_2, 0, 1000, Number_Black),
  % limit(Number_White_2, 0, 1000, Number_White),

% search_black(+Matrix, +Matrix_Changing, +X, +Y)
search_black(_,[],_,_).
search_black(Matrix, [Current_Line|Rest], X, Y):-
  Y2 is Y+1,
  logic_or(nth0(X2, Current_Line, black), nth0(X2, Current_Line, black2)),
  X_Diagonal_Left is X-1,
  X_Diagona_Right is X+1,
  ((X2 == X_Diagonal_Left);(X2 == X_Diagona_Right);(X2 == X)),
  get_element_at(Matrix, X2, Y, Elem1),
  get_element_at(Matrix, X2, Y2, Elem2),
  get_element_at(Matrix, X, Y, Elem3),
  get_element_at(Matrix, X, Y2, Elem4),
  get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black),
  Points_White @=< Points_Black,
  search_black(Matrix, Rest, X2, Y2).

% check_win_black(+Matrix)
check_win_black([First_Row|Rest]):-
  logic_or(nth0(X,First_Row,black), nth0(X,First_Row,black2)),
  Y is 0,
  search_black([First_Row|Rest],Rest, X, Y).

% search_white(+Matrix, +Matrix_Changing, +X, +Y)
search_white(_,[],_,_).
search_white(Matrix, [_Current_Line|Rest] , X, Y):-
  X2 is X+1,
  logic_or(get_element_at(Matrix, X2, Y2, white), get_element_at(Matrix, X2, Y2, white2)),
  Y_Diagonal_Up is Y-1,
  Y_Diagonal_Down is Y+1,
  ((Y2 == Y_Diagonal_Up);(Y2 == Y_Diagonal_Down);(Y2 == Y)),
  get_element_at(Matrix, X2, Y, Elem1),
  get_element_at(Matrix, X2, Y2, Elem2),
  get_element_at(Matrix, X, Y, Elem3),
  get_element_at(Matrix, X, Y2, Elem4),
  get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black),
  Points_Black @=< Points_White,
  search_white(Matrix, Rest , X2, Y2).

% check_win_white(+Matrix)
check_win_white([First_Row|Rest]):-
  X is 0,
  logic_or(get_element_at([First_Row|Rest], X, Y, white), get_element_at([First_Row|Rest], X, Y, white2)),
  search_white([First_Row|Rest], Rest, X, Y).
