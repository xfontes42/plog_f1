append([],L,L).
append([X|L1],L2,[X|L3]):- append(L1,L2,L3).

inverter(Lista,InvLista):- rev(Lista,[],InvLista).
rev([H|T],S,R):- rev(T,[H|S],R).
rev([],R,R).

membro(X,L):- append(_,[X|_],L).

last(L,X):- append(_,[X],L).

nth_membro(1,[M|_],M).
nth_membro(N,[_|T],M):- N>1,
                        N1 is N-1,
                        nth_membro(N1,T,M).

delete_one(X,L,L1):- append(La,[X|Lb],L),
                     append(La,Lb,L1).

before(A,B,L):- append(_,[A|L1],L),
                append(_,[B|_],L1).

ordenada([N]).
ordenada([N1,N2]):- N1 =< N2.
ordenada([N1,N2|Resto]):- N1 =< N2,
                          ordenada([N2|Resto]).

achata_lista([],[]).
achata_lista(X,[X]):- atomic(X).
achata_lista([Cab|Rest],L):- achata_lista(Cab,L1),
                             achata_lista(Rest,L2),
                             append(L1,L2,L). 
