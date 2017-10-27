jogarinicio(X, Y, , Board_In, Board_Out) :- jogar(X, Y, black, Board_In, Board_Out).
jogar(X, Y, Cor, Board_In, Board_Out).
jogar2(X, Y, Cor, Board_In, Board_Out).
checkCrossCut(X, Y, Cor, Board_In, Result).
% partir disto acima para o jogo que queremos



consta_game :- ecra_principal,
               seleciona_modo,
               inicia_jogo,
               ciclo_jogo, %professor falou em algo como um predicado repeat que repete instruçoes
               end_game,
               creditos. % jogo inteiro so para aqui, variavel Board é alterada e passada como parametro
                          %usando o read temos 'pausas' para deixar o jogador decidir onde quer colocar peça/
                                                                        % avançar jogo entre 2 computadores

inicia_jogo :- seleciona_tamanho_tab,
               cria_board_default,

ciclo_jogo :- jogar_inicio,
              repeat,
              jogar,
              verificar_vitoria,
              trocar_jogador%??

jogar :- repeat,
         seleciona_jogada,
         seleciona_local,
         verifica_jogada,%usar if then else para mostrar mensagem de erro
         atualiza_tabuleiro,
         trocar_jogador%??

verifica_jogada :- verifica_limites,
                   verifica_tabuleiro,
                   verifica_crosscuts

seleciona_tamanho_tab(Tamanho) :-repeat,
    write('Selecione um tamanho de board entre 5 e 23.'),
    once(le_numero(Tamanho)),
    ite((Tamanho @>= 5, Tamanho@=< 23),true,(write('Tamanho de Board invalido!'),nl,fail)).

seleciona_jogada(Tipo) :-repeat,
    write('Selecione o tipo de jogada que pretende fazer \'s\' or \'d\' (Single or Double):'),
    once(le_numero(Tipo)),
    ite((Tipo == 52; Tipo == 67),true,(write('Tipo de jogada invalido!'),nl,fail)).

seleciona_local(X,Y) :- repeat,
    write('Selecione uma coordenada X:(_,_)'),
    once(le_numero(X)),
    ite((X @>= 17, X@=<40 ),true,(write('Coordenada X de Board invalida!'),nl,fail)),
    LetraX is X+48,
    repeat,
    write('Selecione uma coordenada Y:('),put_code(LetraX),write(',_)'),
    once(le_numero(Y)),
    ite((Y @>= 0, Y@=<23 ),true,(write('Coordenada Y de Board invalida!'),nl,fail)),
    LetraY is Y,
    write('Coordenadas selecionadas ('),put_code(LetraX),write(', '),write(LetraY),write(').').
