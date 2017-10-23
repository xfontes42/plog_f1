
play_consta :- repeat, nl, presentation,
              write('Choose: '),
              once(read(X)),
              integer(X), X@>0, X@<6,
              nl, write('Good Choice').

% verifyInput(X,Y) :- ((integer(X), integer(Y), X@>0, X@<10, Y@>0, Y@<10) -> true;write('Insira coordenadas validas!'),fail).
%
% test :- repeat, write('Insira as coordenadas(X,Y): '), read(X),write(X), read(Y),write(Y), verifyInput(X,Y).
