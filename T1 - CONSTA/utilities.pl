ite(If, Then, _Else) :- If, !, Then.
ite(_If, _Then, Else) :- Else.

it(If, Then):- If, !, Then.
it(_,_).

copy(Old, New):- functor(Old, F, N), functor(New, F, N).

% Dictionary to associate matrix values.
getCharacter(black, 'b').
getCharacter(black2, 'B').
getCharacter(white, 'w').
getCharacter(white2, 'W').
getCharacter(empty, ' ').
