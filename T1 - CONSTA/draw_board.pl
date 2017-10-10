getCharacter(black, 'b').
getCharacter(black2, 'B').
getCharacter(white, 'w').
getCharacter(white2, 'W').
getCharacter(empty, ' ').

emptyBoard([[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty],
			[empty,empty,empty,empty,empty,empty,empty,empty,empty]]).

printColumnNames :-         write('      A     B     C     D     E     F     G     H     I   '),nl.
printInitialSeparator :- write('    _____________________________________________________ '),nl.
printTopLine :-          write('   |     |     |     |     |     |     |     |     |     |'),nl.
printLineRecursive([]) :- write('|'),nl.
printLineRecursive([Cabeca|Cauda]) :- write('|  '), getCharacter(Cabeca,Simbolo), put_char(Simbolo), write('  '), printLineRecursive(Cauda).
printLineX(Numero,Lista) :- write(' '), X is Numero+48, put_code(X), write(' '), printLineRecursive(Lista).
printFinalSeparator :-   write('   |_____|_____|_____|_____|_____|_____|_____|_____|_____|'),nl.
printFullLine(Numero,Lista) :- printTopLine, printLineX(Numero, Lista), printFinalSeparator.

printBoardRecursive([Cabeca|Cauda],Numero) :- printFullLine(Numero,Cabeca), X is Numero+1, X<10, printBoardRecursive(Cauda,X).
printBoard(Board) :- printColumnNames, printInitialSeparator, printBoardRecursive(Board,1).
