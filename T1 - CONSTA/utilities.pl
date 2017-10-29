ite(If, Then, _Else) :- If, !, Then.
ite(_If, _Then, Else) :- Else.

it(If, Then):- If, !, Then.
it(_,_).

copy(Old, New):- functor(Old, F, N), functor(New, F, N).
