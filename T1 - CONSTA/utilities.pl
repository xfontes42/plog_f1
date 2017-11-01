% ite(+If, +Then, +_Else)
ite(If, Then, _Else) :- If, !, Then.
ite(_If, _Then, Else) :- Else.

% it(+If, +Then)
it(If, Then):- If, !, Then.
it(_,_).

% logic_or(+_X, +_Y)
logic_or(_X, _Y):- _X.
logic_or(_X, _Y):- _Y.

% copy(+Old, -New)
copy(Old, New):- functor(Old, F, N), functor(New, F, N).

% duplicate(+Old, -New)
duplicate(_Old, _New):- fail.
duplicate(_Old, _Old).

% limit(+Value_In, +Bottom_Limit, +Top_Limit, -Value_Out)
limit(Value_In, Bottom_Limit, Top_Limit, Value_Out):-
  Value_In @>= Bottom_Limit,
  Value_In @=< Top_Limit,
  Value_Out is Value_In.
limit(Value_In, Bottom_Limit, Top_Limit, Value_Out):-
  it(
  (Value_In @< Bottom_Limit),
  (Value_Out is Bottom_Limit)
  ),
  it(
  (Value_In @> Top_Limit),
  (Value_Out is Top_Limit)
  ).

% Dictionary to associate matrix values.
getCharacter(black, 'b').
getCharacter(black2, 'B').
getCharacter(white, 'w').
getCharacter(white2, 'W').
getCharacter(empty, ' ').
% Dictionary to associate points.
getPoints(empty, 0).
getPoints(black, 10).
getPoints(black2, 20).
getPoints(white, 1).
getPoints(white2, 2).
% Dictionary to associate elements to its player
getPerson(black, black).
getPerson(black2, black).
getPerson(white, white).
getPerson(white2, white).
getPerson(empty,empty).
% Dictionary to associate current player and play type to the board identifier
%(67+48 = 's'; 52+48 = 'd')
getPlay(52, black, black2).
getPlay(67, black, black).
getPlay(52, white, white2).
getPlay(67, white, white).
