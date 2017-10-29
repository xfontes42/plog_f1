jogarinicio(X, Y, , Board_In, Board_Out) :- jogar(X, Y, black, Board_In, Board_Out).
jogar(X, Y, Cor, Board_In, Board_Out).
jogar2(X, Y, Cor, Board_In, Board_Out).
checkCrossCut(X, Y, Cor, Board_In, Result).
% partir disto acima para o jogo que queremos



consta_game :-
  repeat,
  menu;
  start_game, %DONE
  ciclo_jogo, %professor falou em algo como um predicado repeat que repete instruçoes
  end_game,
  creditos. % jogo inteiro so para aqui, variavel Board é alterada e passada como parametro
            %usando o read temos 'pausas' para deixar o jogador decidir onde quer colocar peça/
            % avançar jogo entre 2 computadores


consta_game :-
  repeat,
  once(menu(Mode)),
  once(it((Mode \== 4,Mode \== 5),(
    start_game(Board),
    ciclo_jogo,
    end_game,
    credits))),
  once(it(Mode == 4, select_dificulty(Dificulty))),
  once(ite(Mode == 5, true,fail)).


menu(Mode) :-
  presentation,
  once(select_game_mode(Mode)),
  it(Mode \== 5,fail).


start_game(Board) :-
  seleciona_tamanho_tab(_Tamanho), %DONE
  create_matrix(_Tamanho,empty,Board), %DONE
  printBoard(Board). %DONE

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

verifica_crosscuts(Matrix,X,Y) :-
  Matrix[X,Y+1]+Matrix[X-1,Y] == Matrix[X,Y]+Matrix[X-1,Y-1]
  Matrix[X,Y-1]+Matrix[X-1,Y] == Matrix[X,Y]+Matrix[X+1,Y-1]
  Matrix[X,Y+1]+Matrix[X+1,Y] == Matrix[X,Y]+Matrix[X-1,Y+1]
  Matrix[X,Y-1]+Matrix[X+1,Y] == Matrix[X,Y]+Matrix[X+1,Y+1]
