% le_numero_enter(13,_Accumulador).
% le_numero_enter(10,_Accumulador).


% le_numero_enter(X) :- name(Lista_Numeros, X), write(Lista_Numeros), write(X).

% le_todos_chars(10,0).
% le_todos_chars(13,0).
% le_todos_chars(Char, [Char| Mais_Chars]) :- get_code(Novo_Char), le_todos_chars(Novo_Char, Mais_Chars).
% le_frase(Frase) :- get_code(Char), le_todos_chars(Char, Lista_Caracteres), name(Frase,Lista_Caracteres).
%


ite(If, Then, _Else) :- If, !, Then.
ite(_If, _Then, Else) :- Else.

le_todos_numeros(Acc, Resultado):-
  get_code(Char),
  ite(
    (Char == 13 ; Char == 10),
    (Resultado is Acc),
    ( Novo_Digito is (Char - 48),
      Novo_Acc is (Acc*10 + Novo_Digito),
      le_todos_numeros(Novo_Acc, Resultado))
      ).

le_numero(Numero) :-
  get_code(Char),
  Digito is (Char - 48),
  le_todos_numeros(Digito, Numero).
