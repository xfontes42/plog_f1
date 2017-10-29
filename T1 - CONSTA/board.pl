:-include('utilities.pl').
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
  length(RowPrefix,X) ,
  append(ColPrefix,[_|ColSufix],Row) ,
  length(ColPrefix,Y) ,
  append(ColPrefix,[Value|ColSufix],RowNew) ,
  append(RowPrefix,[RowNew|RowSufix],New_Matrix).

% valid_move(+Matrix, +X, +Y, +Value)
  valid_move(Matrix, X, Y, Value):-
    get_element_at(Matrix, X, Y, Current_Piece)
    Current_Piece \== black2, Current_Piece \== white2, % Cannot play on top of a double
    current_player(X), ((X == Current_Piece, X == Value); Current_Piece == empty). % Cannot play different colors or a double over a single
    % So falta verificar crosscuts


% replace a single cell in a list-of-lists
% - the source list-of-lists is L
% - The cell to be replaced is indicated with a row offset (X)
%   and a column offset within the row (Y)
% - The replacement value is Z
% - the transformed list-of-lists (result) is R
% replace( L , X , Y , Z , R ) :-
%   append(RowPfx,[Row|RowSfx],L),     % decompose the list-of-lists into a prefix, a list and a suffix
%   length(RowPfx,X) ,                 % check the prefix length: do we have the desired list?
%   append(ColPfx,[_|ColSfx],Row) ,    % decompose that row into a prefix, a column and a suffix
%   length(ColPfx,Y) ,                 % check the prefix length: do we have the desired column?
%   append(ColPfx,[Z|ColSfx],RowNew) , % if so, replace the column with its new value
%   append(RowPfx,[RowNew|RowSfx],R).   % and assemble the transformed list-of-lists
% at StackOverflow
