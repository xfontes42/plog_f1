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

% get_path_points_black(+Matrix, +Secondary_Matrix, +X_Black, +Current_Y, ?Current_Points)
get_path_points_black(_,[],_,_,Current_Points):-
  update_max_points(Current_Points).
  % New_Current_Points is Current_Points + 1,
  % update_max_points(New_Current_Points).

get_path_points_black(Matrix, [Row_Ahead_Of_Current_Y| Rest], X , Current_Y, Current_Points):-
  New_Y is Current_Y + 1,
  X_Diagonal_Left is X-1,
  X_Diagonal_Right is X+1,
  ite(
    % IF
    (logic_or(nth0(X_Found,Row_Ahead_Of_Current_Y,black), nth0(X_Found, Row_Ahead_Of_Current_Y, black2))),
    % THEN
    (
      logic_or(nth0(X_Found_2,Row_Ahead_Of_Current_Y,black), nth0(X_Found_2, Row_Ahead_Of_Current_Y, black2)),
      ite(
        % IF
        ((X_Found_2 == X_Diagonal_Left);(X_Found_2 == X_Diagonal_Right);(X_Found_2 == X)),
        % THEN
        (
          % Verificar se ganha possivel crosscut
          get_element_at(Matrix, X_Found_2, New_Y, Elem1),
          get_element_at(Matrix, X_Found_2, Current_Y, Elem2),
          get_element_at(Matrix, X, New_Y, Elem3),
          get_element_at(Matrix, X, Current_Y, Elem4),
          get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black),
          ite(
            % IF
            (Points_White @=< Points_Black),
            % THEN - black can move forward
            (
              New_Current_Points is Current_Points+1,
              update_max_points(New_Current_Points),
              get_path_points_black(Matrix, Rest, X_Found_2, New_Y, New_Current_Points)
            ),
            % ELSE - black is not in control, move forward but reset max
            (
              get_path_points_black(Matrix, Rest, X_Found_2, New_Y, 1)
            )
          )
        ),
        % ELSE - found black pieces but not on a path
        (
          get_path_points_black(Matrix, Rest, X_Found_2, New_Y, 1)
        )
      ),
      fail
    ),
    % ELSE - did not find black pieces in this row, restart
    (New_Y_Rest is New_Y + 1, searh_black_points(Matrix, Rest, New_Y_Rest))
  ).

% searh_black_points(+Matrix, +Secondary_Matrix, Current_Line)
searh_black_points(_, [], _).
searh_black_points(Matrix,[Current_Row|Rest],Current_Y):-
    ite(
    % IF
    ((nth0(X_Found,Current_Row,black);nth0(X_Found,Current_Row,black2))),
    % THEN
    ( (nth0(X_Found_2,Current_Row,black);nth0(X_Found_2,Current_Row,black2)),
      update_max_points(1),
      get_path_points_black(Matrix,Rest,X_Found_2,Current_Y,1),
      fail
    ),
    % ELSE
    (
      Next_Y is Current_Y +1, searh_black_points(Matrix,Rest,Next_Y))
    ).

% eval_board_black(+Matrix, -Number_Black)
eval_board_black(Matrix, Number_Black):-
  \+searh_black_points(Matrix, Matrix, 0),
  max_points(Number_Black).

% get_path_points_black(+Matrix, +Secondary_Matrix, +X_Black, +Current_Y, ?Current_Points)
get_path_points_white(_,[],_,_,Current_Points):-
  update_max_points(Current_Points).
get_path_points_white(Matrix, [_Row_Ahead_Of_Current_Y| Rest], Current_X , Y, Current_Points):-
  New_X is Current_X + 1,
  Y_Diagonal_Up is Y-1,
  Y_Diagonal_Down is Y+1,
  ite(
    % IF
    (logic_or(get_element_at(Matrix, New_X, Y_Found_2, white), get_element_at(Matrix, New_X, Y_Found_2, white2))),
    % THEN
    (
      logic_or(get_element_at(Matrix, New_X, Y_Found, white), get_element_at(Matrix, New_X, Y_Found, white2)),
      ite(
        % IF
        ((Y_Found == Y_Diagonal_Up);(Y_Found == Y_Diagonal_Down);(Y_Found == Y)),
        % THEN
        (
          % Verificar se ganha possivel crosscut
          get_element_at(Matrix, Current_X, Y, Elem1),
          get_element_at(Matrix, Current_X, Y_Found, Elem2),
          get_element_at(Matrix, New_X, Y, Elem3),
          get_element_at(Matrix, New_X, Y_Found, Elem4),
          get_points_from_square(Elem1, Elem2, Elem3, Elem4, Points_White, Points_Black),
          ite(
            % IF
            (Points_Black @=< Points_White),
            % THEN - black can move forward
            (
              New_Current_Points is Current_Points+1,
              update_max_points(New_Current_Points),
              get_path_points_white(Matrix, Rest, New_X, Y_Found, New_Current_Points)
            ),
            % ELSE - black is not in control, move forward but reset max
            (
              get_path_points_white(Matrix, Rest, New_X, Y_Found, 1)
            )
          )
        ),
        % ELSE - found black pieces but not on a path
        (
          get_path_points_white(Matrix, Rest, New_X, Y_Found, 1)
        )
      ),
      fail
    ),
    % ELSE - did not find white pieces in this column, restart
    (New_X_Rest is New_X + 1, searh_white_points(Matrix, Rest, New_X_Rest))
  ).

% searh_white_points(+Matrix, +Secondary_Matrix, Current_Line)
searh_white_points(_, [], _).
searh_white_points(Matrix,[_Current_Row|Rest],Current_X):-
  ite(
  % IF
  (logic_or(get_element_at(Matrix, Current_X, Y_Found, white), get_element_at(Matrix, Current_X, Y_Found, white2))),
  % THEN
  ( logic_or(get_element_at(Matrix, Current_X, Y_Found_2, white), get_element_at(Matrix, Current_X, Y_Found_2, white2)),
    update_max_points(1),
    get_path_points_white(Matrix,Rest,Current_X,Y_Found_2,1),
    fail
  ),
  % ELSE
  (Next_X is Current_X +1, searh_white_points(Matrix,Rest,Next_X))
  ).

% eval_board_white(+Matrix, -Number_White)
eval_board_white(Matrix, Number_White):-
  \+searh_white_points(Matrix, Matrix, 0),
  max_points(Number_White).

% eval_board(+Matrix, -Number_Black, -Number_White)
eval_board(Matrix, Number_Black, Number_White):-
  set_max_points(0),
  eval_board_black(Matrix, Number_Black),
  % write('passei black'), nl,
  set_max_points(0),
  eval_board_white(Matrix, Number_White),
  % write('passei white'), nl,
  set_max_points(0).
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

% eval_board([[ white, empty, white, empty, black],
%             [ empty, white, black2, empty, empty],
%             [ empty, black, empty, empty, empty],
%             [ empty, empty, empty, empty, empty],
%             [ empty, empty, empty, empty, black]], X2, Y2).
