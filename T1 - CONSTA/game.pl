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
