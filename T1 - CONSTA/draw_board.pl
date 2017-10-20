getCharacter(black, 'b').
getCharacter(black2, 'B').
getCharacter(white, 'w').
getCharacter(white2, 'W').
getCharacter(empty, ' ').

boards(empty,[[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty]]).

boards(earlygame,[[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[white2,empty,empty,empty,empty,empty,empty,white,empty],
			[empty,empty,empty,empty,black2,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,black,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty]]).

boards(endgame,[[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,black,empty,empty],
			[empty,empty,empty,empty,white,white2,black,empty,empty],
			[white2,white,white,white,white,black2,white2,white,white],
			[empty,empty,empty,empty,black2,black,empty,empty,empty],
			[empty,empty,empty,empty,empty,black,empty,empty,empty],
			[empty,empty,empty,empty,empty,black,black,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty]]).


printColumnNames :-      write('      A     B     C     D     E     F     G     H     I   '),nl.
printInitialSeparator :- write('    _____________________________________________________ '),nl.
printTopLine :-          write('   |     |     |     |     |     |     |     |     |     |'),nl.
printFinalSeparator :-   write('   |_____|_____|_____|_____|_____|_____|_____|_____|_____|'),nl.

printLineRecursive([]) :- write('|'),nl.
printLineRecursive([Cabeca|Cauda]) :- write('|  '), getCharacter(Cabeca,Simbolo), put_char(Simbolo), write('  '), printLineRecursive(Cauda).
printLineX(Numero,Lista) :- write(' '), X is Numero+48, put_code(X), write(' '), printLineRecursive(Lista).
printFullLine(Numero,Lista) :- printTopLine, printLineX(Numero, Lista), printFinalSeparator.
printBoardRecursive([Cabeca|Cauda],Numero) :- printFullLine(Numero,Cabeca), X is Numero+1, X<10, printBoardRecursive(Cauda,X).
printBoard(Type) :- boards(Type,Board), printColumnNames, printInitialSeparator, printBoardRecursive(Board,1).


verifyInput(X,Y) :- ((integer(X), integer(Y), X@>0, X@<10, Y@>0, Y@<10) -> true;write('Insira coordenadas validas!'),fail).

test :- repeat, write('Insira as coordenadas(X,Y): '), read(X),write(X), read(Y),write(Y), verifyInput(X,Y).
