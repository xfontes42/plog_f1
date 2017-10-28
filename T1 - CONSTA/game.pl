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
