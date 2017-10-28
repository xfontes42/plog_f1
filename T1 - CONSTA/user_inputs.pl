:-include('utilities.pl').

le_todos_chars(10,0).
le_todos_chars(13,0).
le_todos_chars(Char, [Char| Mais_Chars]) :- get_code(Novo_Char), le_todos_chars(Novo_Char, Mais_Chars).
le_frase(Frase) :- get_code(Char), le_todos_chars(Char, Lista_Caracteres), name(Frase,Lista_Caracteres).

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


seleciona_tamanho_tab(Tamanho) :-repeat,
    write('Selecione um tamanho de board entre 5 e 25.'),
    once(le_numero(Tamanho)),
    ite((Tamanho @>= 5, Tamanho@=< 25),(true),(write('Tamanho de Board invalido!'),nl,fail)).

seleciona_jogada(Tipo) :-repeat,
    write('Selecione o tipo de jogada que pretende fazer \'s\' or \'d\' (Single or Double):'),
    once(le_numero(Tipo)),
    ite((Tipo == 52; Tipo == 67),true,(write('Tipo de jogada invalido!'),nl,fail)).

seleciona_local(X,Y) :-
    repeat,
      write('Selecione uma coordenada X:(_,_)'),
      once(le_numero(Temp_X)),
      ite((Temp_X @>= 17, Temp_X@=<42 ),true,(write('Coordenada X de Board invalida!'),nl,fail)),
      X is Temp_X - 17,
      LetraX is Temp_X+48,
    repeat,
      write('Selecione uma coordenada Y:('),put_code(LetraX),write(',_)'),
      once(le_numero(Y)),
      ite((Y @>= 0, Y@=<25 ),true,(write('Coordenada Y de Board invalida!'),nl,fail)),
      LetraY is Y,
      write('Coordenadas selecionadas ('),put_code(LetraX),write(', '),write(LetraY),write(').').
