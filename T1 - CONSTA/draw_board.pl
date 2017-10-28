:-include('utilities.pl').

% Dictionary to associate matrix values.
getCharacter(black, 'b').
getCharacter(black2, 'B').
getCharacter(white, 'w').
getCharacter(white2, 'W').
getCharacter(empty, ' ').

% Print header.
printHeader(_,0):- write(' '), nl.
printHeader(Current,Missing):-
	write('   '),
	Letter is Current+65,
	put_code(Letter),
	write('  '),
	Current2 is Current+1,
	Missing2 is Missing-1,
	printHeader(Current2,Missing2).

% Prints the initial separator of the board.
printInitialSeparator(0):- write(' '), nl.
printInitialSeparator(Number):-
	write('_'),
	Number2 is Number-1,
	printInitialSeparator(Number2).

% Prints the top of a whole line.
printTopLine(0):- write('|'), nl.
printTopLine(Number):-
	write('|     '),
	Number2 is Number-1,
	printTopLine(Number2).

% Prints the line given recursively.
printLineRecursive([]) :- write('|'),nl.
printLineRecursive([Cabeca|Cauda]) :-
	write('|  '),
	getCharacter(Cabeca,Simbolo),
	put_char(Simbolo), write('  '),
	printLineRecursive(Cauda).

% Prints a given line.
printLineX(Numero,Lista) :-
	ite( (Numero @< 10), (write(' ')), (write('') )),
	write(Numero),
	write(' '),
	printLineRecursive(Lista).

% Prints the bottom of a whole line.
printFinalSeparator(0):- write('|'), nl.
printFinalSeparator(Number):-
	write('|_____'),
	Number2 is Number-1,
	printFinalSeparator(Number2).

% Print a whole line of the board.
printFullLine(Numero,Lista) :-
	length(Lista, Length_Lista),
	write('   '),
	printTopLine(Length_Lista),
	printLineX(Numero, Lista),
	write('   '),
	printFinalSeparator(Length_Lista).

% Recursive function to print each line given.
printBoardRecursive([],_).
printBoardRecursive([Cabeca|Cauda],Numero) :-
	printFullLine(Numero,Cabeca),
	X is Numero+1,
	printBoardRecursive(Cauda,X).

% Print the whole board.
printBoard(Matrix):-
	length(Matrix, Length),
	write('   '),
	printHeader(0,Length),
	write('    '),
	Length_Underscore is ((Length*6)-1),
	printInitialSeparator(Length_Underscore),
	printBoardRecursive(Matrix,0).
