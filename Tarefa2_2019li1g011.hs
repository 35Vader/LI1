{- | Module      : Tarefa2_2019li1g011
   | Escola      : Universidade Do Minho
   | Description : Efetuar jogadas
   | Copyright   : Alexandra Dias Candeias <a89521@alunos.uminho.pt>
                   Francisco Correia Franco <a89458@alunos.uminho.pt>

= Introdução Tarefa 2:
Nesta tarefa o nosso principal alvo foi o efeito de uma determinada jogada no estado do jogo relativamente à sua descrição.

= Objetivos e Estratégias utilizadas:
Inicialmente pensamos que para o jogo efetuar as jogadas era necessário saber certos aspetos do jogo. Com isto, achamos melhor calcular a inclinação de um certo bloco no mapa, tal como a altura de uma posição no mesmo. 
Também decidimos retribuir um bloco numa certa posição tal como na pista, como no mapa. Foi também essencial colocar cola num determinado bloco do mapa e da pista, bem como receber o número de pistas de um mapa. 
Depois escolhemos efetuar as jogadas possíveis dos jogadores. Isto é, as jogadas como a Movimenta (D, E, B e C), a Acelera e Desacelera. Jogadas como a Dispara, quer nos outros jogadores, quer no próprio mapa, também foram efetuadas nesta tarefa.
Por fim, juntamos tudo numa função jogada com o objetivo de escolher qual o jogador que iria efetuar a jogada em questão, assim como a própria jogada e o estado anterior e o resultante após o jogador efetuar o seu objetivo.

= Conclusão:
Resumindo, com esta tarefa o nosso grupo teve como principal foco efetuar a jogada proposta por um determinado jogador.

-}

-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g011 where

import Tarefa1_2019li1g011
import Tarefa0_2019li1g011
import LI11920

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(0, Movimenta D, Estado testmap [(Jogador 3 3 0 0 (Chao True))]),
            (2, Movimenta B, Estado testmap [(Jogador 2 1.4 2 5 (Chao False)), (Jogador 0 4.2 2 1 (Morto 3)),(Jogador 1 3 0 0 (Chao True))]),
            (1, Movimenta E, Estado testmap [(Jogador 3 2 3.2 0 (Chao True)),(Jogador 1 3 0 0 (Morto 2))]),
            (0, Acelera, Estado testmap [(Jogador 2 3 1.5 3 (Chao False)),(Jogador 0 3 0 0 (Chao True)),(Jogador 2 3 0 0 (Chao True))]),
            (0, Movimenta D, Estado testmap [(Jogador 2 4 1.2 0 (Chao True))]),
            (2, Desacelera, Estado testmap [(Jogador 1 3.2 1.3 2 (Chao False)),(Jogador 3 2.3 1 0 (Chao False)),(Jogador 2 2.6 1.7 4 (Chao True))]),
            (1, Acelera, Estado testmap [(Jogador 1 3 0 0 (Chao True)),(Jogador 3 3 0 0 (Chao True))]),
            (3, Movimenta C, Estado testmap [(Jogador 3 2 0 0 (Chao True)),(Jogador 2 3 0 0 (Chao True)),(Jogador 2 3.2 1.5 3 (Chao False)),(Jogador 0 3 3 0 (Morto 1.3))]),
            (0, Dispara, Estado testmap [(Jogador 1 4.2 0 0 (Chao True))]),
            (0, Dispara, Estado testmap [(Jogador 2 3 0 0 (Chao True))]),
            (1, Movimenta D, Estado testmap [(Jogador 3 3 0 0 (Chao True)),(Jogador 1 2.8 1.6 4 (Chao True))]),
            (0, Desacelera, Estado testmap [(Jogador 2 3 2.1 0 (Chao False)),(Jogador 1 3 0 3 (Morto 2.3))]),
            (2, Dispara, Estado testmap [(Jogador 2 3 0 0 (Chao False)),(Jogador 1 3 0 0 (Chao False)),(Jogador 3 3 0 0 (Chao True))])]


testmap = gera 4 6 4

-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada id_jogador (Movimenta E) (Estado mapa jogadores) = Estado mapa (jogadaMovimentaE id_jogador jogadores)
jogada id_jogador (Movimenta D) (Estado mapa jogadores) = Estado mapa (jogadaMovimentaD id_jogador jogadores)
jogada id_jogador (Movimenta C) (Estado mapa jogadores) = Estado mapa (jogadaMovimentaC id_jogador jogadores mapa)
jogada id_jogador (Movimenta B) (Estado mapa jogadores) = Estado mapa (jogadaMovimentaB id_jogador jogadores mapa)
jogada id_jogador Acelera (Estado mapa jogadores) = Estado mapa (jogadaAcelera id_jogador jogadores)
jogada id_jogador Desacelera (Estado mapa jogadores) = Estado mapa (jogadaDesacelera id_jogador jogadores)
jogada id_jogador Dispara (Estado mapa jogadores) = Estado (jogadaDisparaMapa id_jogador mapa jogadores) (jogadaDisparaJogadores id_jogador jogadores)


-- | Efetua a jogada Dispara no mapa
jogadaDisparaMapa :: Int -- ^ Identificador do jogador que efetua a jogada
                  -> Mapa -- ^ Mapa do jogo antes da jogada
                  -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                  -> Mapa -- ^ Mapa do jogo depois da jogada
jogadaDisparaMapa 0 mapa (Jogador _ _ _ 0 _ : js) = mapa
jogadaDisparaMapa 0 mapa (Jogador p d _ _ (Chao _) : js) = if d >= 1 then poeColaNoBlocoMapa p ((floor d)-1) mapa else mapa
jogadaDisparaMapa 0 mapa _ = mapa
jogadaDisparaMapa id_jogador mapa (j:js) = jogadaDisparaMapa (id_jogador-1) mapa js

-- | Efetua a jogada Dispara nos jogadores
jogadaDisparaJogadores :: Int -- ^ Identificador do jogador que efetua a jogada
                       -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                       -> [Jogador] -- ^ Lista dos jogadores depois da jogada
jogadaDisparaJogadores 0 (Jogador p d v 0 e : js) = Jogador p d v 0 e : js
jogadaDisparaJogadores 0 (Jogador p d v c (Chao acel) : js) = if d >= 1 then Jogador p d v (c-1) (Chao acel) : js else Jogador p d v c (Chao acel) : js
jogadaDisparaJogadores 0 js = js
jogadaDisparaJogadores id_jogador (j:js) = j : jogadaDisparaJogadores (id_jogador-1) js

-- | Efetua a jogada Desacelera
jogadaDesacelera :: Int -- ^ Identificador do jogador que efetua a jogada
                 -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                 -> [Jogador] -- ^ Lista dos jogadores depois da jogada
jogadaDesacelera 0 (Jogador p d v c (Chao acel) : js) = (Jogador p d v c (Chao False)) : js
jogadaDesacelera 0 js = js
jogadaDesacelera id_jogador (j:js) = j : jogadaDesacelera (id_jogador-1) js

-- | Efetua a jogada Acelera
jogadaAcelera :: Int -- ^ Identificador do jogador que efetua a jogada
              -> [Jogador] -- ^ Lista dos jogadores antes da jogada
              -> [Jogador] -- ^ Lista dos jogadores depois da jogada
jogadaAcelera 0 (Jogador p d v c (Chao acel) : js) = (Jogador p d v c (Chao True)) : js
jogadaAcelera 0 js = js
jogadaAcelera id_jogador (j:js) = j : jogadaAcelera (id_jogador-1) js

-- | Efetua a jogada Movimenta C
jogadaMovimentaC :: Int -- ^ Identificador do jogador que efetua a jogada
                 -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                 -> Mapa -- ^ Mapa do jogo antes da jogada
                 -> [Jogador] -- ^ Lista dos jogadores depois da jogada
jogadaMovimentaC 0 (Jogador 0 d v c e : js) _ = (Jogador 0 d v c e) : js
jogadaMovimentaC 0 (Jogador p d v c (Chao acel) : js) mapa = let bloco_jogador = getElemMapa p (floor d) mapa
                                                                 altura_jogador = getAltura bloco_jogador (d - fromInteger(floor d))
                                                                 bloco_movimento = getElemMapa (p-1) (floor d) mapa
                                                                 altura_bloco = getAltura bloco_movimento (d - fromInteger(floor d))
                                                             in if altura_jogador - altura_bloco < -0.2 then (Jogador p d v c (Morto 1)) : js
                                                                else if altura_jogador - altura_bloco > 0.2 then (Jogador (p-1) d v c (Ar altura_jogador (getInclinacao (getElemMapa p (floor d) mapa)) 0)) : js
                                                                else (Jogador (p-1) d v c (Chao acel)) : js
jogadaMovimentaC 0 js _ = js
jogadaMovimentaC id_jogador (j:js) mapa = j : jogadaMovimentaC (id_jogador-1) js mapa

-- | Efetua a jogada Movimenta B
jogadaMovimentaB :: Int -- ^ Identificador do jogador que efetua a jogada
                 -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                 -> Mapa -- ^ Mapa do jogo antes da jogada
                 -> [Jogador] -- ^ Lista dos jogadores depois da jogada
jogadaMovimentaB 0 (Jogador p d v c (Chao acel) : js) mapa = if p+1 == nPistas mapa then (Jogador p d v c (Chao acel) : js)
                                                             else let bloco_jogador = getElemMapa p (floor d) mapa
                                                                      altura_jogador = getAltura bloco_jogador (d - fromInteger(floor d))
                                                                      bloco_movimento = getElemMapa (p+1) (floor d) mapa
                                                                      altura_bloco = getAltura bloco_movimento (d - fromInteger(floor d))
                                                                  in if altura_jogador - altura_bloco < -0.2 then (Jogador p d v c (Morto 1)) : js
                                                                     else if altura_jogador - altura_bloco > 0.2 then (Jogador (p+1) d v c (Ar altura_jogador (getInclinacao (getElemMapa p (floor d) mapa)) 0)) : js
                                                                     else (Jogador (p+1) d v c (Chao acel)) : js
--jogadaMovimentaB 0 (Jogador p d v c (Chao acel) : js) mapa = if p+1 >= nPistas mapa then (Jogador p d v c (Chao acel)) : js
--                                                             else if abs (altura_jogador - altura_bloco) <= 0.2 then (Jogador (p-1) d v c (Chao acel)) : js else (Jogador p d v c (Chao acel)) : js
--                                                           where bloco_jogador = getElemMapa p (floor d) mapa
--                                                                 altura_jogador = getAltura bloco_jogador (d - fromInteger(floor d))
--                                                                 bloco_movimento = getElemMapa (p-1) (floor d) mapa
--                                                                 altura_bloco = getAltura bloco_movimento (d - fromInteger(floor d))
jogadaMovimentaB 0 js _ = js
jogadaMovimentaB id_jogador (j:js) mapa = j : jogadaMovimentaB (id_jogador-1) js mapa

-- | Efetua a jogada Movimenta E
jogadaMovimentaE :: Int -- ^ Identificador do joget
                 -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                 -> [Jogador] -- ^ Lista dos jogadores após a jogada
jogadaMovimentaE 0 (Jogador p d v c (Ar alt inc grav) : js) = let inc_norm = normalizaAngulo inc
                                                              in if normalizaAngulo(inc_norm+15) > 90 then (Jogador p d v c (Ar alt 90 grav)) : js 
                                                                 else (Jogador p d v c (Ar alt (inc_norm+15) grav)) : js
jogadaMovimentaE 0 js = js
jogadaMovimentaE id_jogador (j:js) = j : jogadaMovimentaE (id_jogador-1) js

-- | Efetua a jogada Movimenta D
jogadaMovimentaD :: Int -- ^ Identificador do jogador que efetua a jogada
                 -> [Jogador] -- ^ Lista dos jogadores antes da jogada
                 -> [Jogador] -- ^ Lista dos jogadores após a jogada
jogadaMovimentaD 0 (Jogador p d v c (Ar alt inc grav) : js) = let inc_norm = normalizaAngulo inc
                                                              in if normalizaAngulo(inc_norm-15) < 270 then (Jogador p d v c (Ar alt (-90) grav)) : js
                                                                 else (Jogador p d v c (Ar alt (inc_norm-15-360) grav)) : js
jogadaMovimentaD 0 js = js
jogadaMovimentaD id_jogador (j:js) = j : jogadaMovimentaD (id_jogador-1) js


-- * Funções auxiliares da Tarefa 2

-- | Retribui o número de pistas de um mapa
nPistas :: Mapa -- ^ Mapa do jogo
        -> Int -- ^ Número de pistas do mapa
nPistas [] = 0
nPistas (p:ps) = 1 + nPistas ps

-- | Retribui um bloco numa determinada posição no mapa
getElemMapa :: Int -- ^ Linha do bloco
            -> Int -- ^ Coluna do bloco
            -> Mapa -- ^ Mapa onde se encontra o bloco
            -> Peca -- ^ Bloco na posiçao (Linha,Coluna)
getElemMapa 0 c (p:ps) = getElemPista c p
getElemMapa l c (p:ps) = getElemMapa (l-1) c ps

-- | Retribui um bloco numa determinada posição na pista
getElemPista :: Int -- ^ Posição do bloco
             -> Pista -- ^ Pista onde se encontra o bloco
             -> Peca -- ^ Bloco a ser retribuido
getElemPista 0 (b:bs) = b
getElemPista c (b:bs) = getElemPista (c-1) bs

-- | Retribui a altura de uma posição do mapa
getAltura :: Peca -- ^ Bloco do mapa
          -> Double -- ^ Posição no bloco
          -> Double -- ^ Altura da posição no bloco
getAltura (Recta _ x) _ = fromIntegral(x)
getAltura (Rampa _ x y) d = fromIntegral(x) + d * ((fromIntegral(y) - fromIntegral(x)))

-- | Coloca cola num determinado bloco do mapa
poeColaNoBlocoMapa :: Int -- ^ Linha do bloco
                   -> Int -- ^ Coluna do bloco
                   -> Mapa -- ^ Mapa onde se encontra o bloco
                   -> Mapa -- ^ Mapa com o bloco atualizado
poeColaNoBlocoMapa 0 c (p:ps) = poeColaNoBlocoPista c p : ps
poeColaNoBlocoMapa l c (p:ps) = p : poeColaNoBlocoMapa (l-1) c ps

-- | Coloca cola num determinado bloco da pista
poeColaNoBlocoPista :: Int -- ^ Posição do bloco
                    -> Pista -- ^ Pista onde se encontra o bloco
                    -> Pista -- ^ Pista com o bloco atualizado
poeColaNoBlocoPista 0 (Recta _ x : bs) = Recta Cola x : bs
poeColaNoBlocoPista 0 (Rampa _ x y : bs) = Rampa Cola x y : bs
poeColaNoBlocoPista c (b:bs) = b : poeColaNoBlocoPista (c-1) bs

-- | Retribui a inclinação de um dado bloco do mapa
getInclinacao :: Peca -- ^ Peça do mapa
              -> Double -- ^ Inclinação da peça recebida
getInclinacao (Recta _ _) = 0
getInclinacao (Rampa _ x1 x2) = atan(fromIntegral(x2-x1)) * 180.0 / pi

