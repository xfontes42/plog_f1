% points(empty, 0).
% points(black, 100).
% points(black2, 150).
% points(white, -100).
% points(white2, -150).
%
%
% stuff_line([],[],[]).
% stuff_line([First_Point|Rest_Points],[First_Piece|Rest_Pieces],[First_Result|Rest_Results]):-
%   points(First_Piece, Result),
%   First_Result is First_Point + Result,
%   stuff_line(Rest_Points, Rest_Pieces, Rest_Results).
%
%
% stuff_matrix([],[],[]).
% stuff_matrix([First_Line_Points|Rest_Points],
%              [First_Line_Matrix|Rest_Matrix],
%              [First_Line_Result|Rest_Result]):-
%                stuff_line(First_Line_Points,First_Line_Matrix,First_Line_Result),
%                stuff_matrix(Rest_Points, Rest_Matrix, Rest_Result).
%
% apply_force_field(Matrix, X, Y, Value):-
%   ite(
%   (
%     get_element_at(Matrix,X,Y,Piece), Piece \== empty
%   ),
%   (
%     get_element_at(Matrix,X,Y,Piece2), points(Piece2, Res),
%     Value is Res
%   ),
%   (
%     Value is 0.05
%   )
%   ).
%
%
% stuff_force_field_line([], _, _, _, _).
% stuff_force_field_line([_Element|Rest], Points_In, Matrix, Points_Out, X, Y):-
%   apply_force_field(Matrix, X, Y, Value),
%   set_element_at(Points_In, X, Y, Value, Points_Temp),
%   X2 is X+1,
%   stuff_force_field_line(Rest, Points_Temp, Matrix, Points_Out, X2, Y).
%
%
% stuff_force_field_matrix([], _, _, _, _).
% stuff_force_field_matrix([Row_Aux|Rest_Aux], Points_In, Matrix, Points_Out, Y):-
%   stuff_force_field_line(Row_Aux, Points_In, Matrix, Points_Temp, 0, Y),
%   Y2 is Y+1,
%   stuff_force_field_matrix(Rest_Aux, Points_Temp, Matrix, Points_Out, Y2).
%
%
% eval_board_2(Matrix, _Number_White, _Number_Black):-
%   length(Matrix, Size_Matrix),
%   create_matrix(Size_Matrix, 0, Points_Matrix),
%   stuff_matrix(Points_Matrix, Matrix, Matrix_Temp),
%
%   stuff_force_field_matrix(Matrix_Temp, Points_Matrix, Matrix, Resulting_Matrix, 0)
%   .
%
% % example1
% teste :- eval_board_2([[black, empty, empty, empty, empty],
%  [empty, empty, empty, empty, empty],
%  [empty, empty, empty, empty, empty],
%  [empty, empty, empty, empty, empty],
%  [empty, empty, empty, empty, empty]], _N1, _N2).
