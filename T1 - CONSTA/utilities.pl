ite(If, Then, _Else) :- If, !, Then.
ite(_If, _Then, Else) :- Else.

it(If, Then):- If, !, Then.
it(_,_).

logic_or(_X, _Y):- _X.
logic_or(_X, _Y):- _Y.

copy(Old, New):- functor(Old, F, N), functor(New, F, N).

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
