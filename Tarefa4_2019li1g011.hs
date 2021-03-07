{- | Module      : Tarefa4_2019li1g011
   | Escola      : Universidade Do Minho
   | Description : Reagir à passagem do tempo
   | Copyright   : Alexandra Dias Candeias <a89521@alunos.uminho.pt>
                   Francisco Correia Franco <a89458@alunos.uminho.pt>

= Introdução Tarefa 4:
Com esta tarefa nós conseguimos calcular o efeito da passagem instantânea do tempo num determinado estado de jogo.

= Objetivos e Estratégias utilizadas:
O nosso objetivo nesta tarefa foi calcular, num período de tempo especifico do jogo, a passagem do tempo num instante.
Primeiro era necessário calcular a velocidade de um jogador após passar um determinado tempo, ou seja, após a passagem de x segundos. 
Concordamos que era melhor retribuir a altura final, bem como o piso e o atrito, de uma peça no mapa. Foi também preciso calcular a abcissa de um certo ponto. 
Depois, achamos importante perceber como altera a posição de um jogador durante um período te tempo estabelecido, daí a termos criado uma função com esse mesmo propósito, 
sendo fundamental determinar um certo tempo decorrido, bem como o mapa utilizado e o estado pós e pré do jogador.
Foi também essencial avançar o estado de um jogador, isto é, atualizar o seu estado um “passo em frente” à medida que ía passando o tempo. 
Alteramos, também, a velocidade de um determinado jogador durante o tempo de jogo.

= Conclusão:
Em conclusão, queríamos, com esta tarefa, perceber como a passagem do tempo afetava a dinâmica do jogo, da mesma forma que as alterações do jogo e dos jogadores. 

-}
-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g011 where

import LI11920
import Tarefa0_2019li1g011
import Tarefa2_2019li1g011

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 = [(2, testmap, Jogador 1 1.7 2 0 (Ar 3 20 0)),
             (5, testmap,Jogador 3 3 0 0 (Chao True)),
             (10, testmap, Jogador 1 3.2 1.3 2 (Chao False)),
             (3, testmap, Jogador 0 2.2 1.5 2 (Ar 2 15 4)),
             (7, testmap, Jogador 2 3 1.5 3 (Chao False)),
             (15, testmap, Jogador 3 3 0 0 (Chao True)),
             (20, testmap, Jogador 1 3 0 3 (Morto 2.3)),
             (9, testmap, Jogador 1 2.3 1 2 (Ar 3.5 5 2)),
             (13, testmap, Jogador 0 1.7 2 0 (Ar 2.1 8 1)),
             (2, testmap, Jogador 2 3 0 0 (Chao True)),
             (4, testmap, Jogador 1 3 0 0 (Morto 2)),
             (18, testmap, Jogador 3 4 2 1 (Chao True)),
             (30, testmap, Jogador 3 1.7 2 0 (Ar 3 20 0)),
             (27, testmap, Jogador 2 3 2.1 0 (Chao False)),
             (28, testmap, Jogador 1 2.8 1.6 4 (Chao True)),
             (38, testmap, Jogador 1 2.4 1 3 (Ar 5 23 5)),
             (18, testmap, Jogador 2 2.6 1.7 4 (Chao True)),
             (25, testmap, Jogador 3 2.3 1 0 (Chao False))]


-- * Funções principais da Tarefa 4.

-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)


-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera tempo mapa (Jogador pista dist velo cola (Morto cooldown)) = Jogador pista dist velo cola (Morto cooldown)
acelera tempo mapa (Jogador pista dist velo cola (Chao acel)) = let velo_atualizado = calculaVelocidadeChao tempo velo acel (getAtrito (getElemMapa pista (floor dist) mapa))
                                                                in Jogador pista dist velo_atualizado cola (Chao acel)
acelera tempo mapa (Jogador pista dist velo cola (Ar alt incl grav)) = let velo_atualizado = velo - (0.125 * velo * tempo)
                                                                           grav_atualizada = grav + tempo
                                                                       in if velo_atualizado >= 0 then Jogador pista dist velo_atualizado cola (Ar alt incl grav_atualizada)
                                                                                                  else Jogador pista dist 0 cola (Ar alt incl grav_atualizada)


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move tempo mapa (Jogador pista dist velo cola (Morto cooldown)) = if cooldown - tempo > 0 then Jogador pista dist velo cola (Morto (cooldown-tempo))
                                                                                          else Jogador pista dist velo cola (Chao False)
                                                                                                                           
move tempo mapa (Jogador pista dist velo cola (Chao acel)) = let dist_atualizada = dist + (velo * tempo)
                                                             in if dist_atualizada < (fromIntegral(floor dist)+1) then Jogador pista dist_atualizada velo cola (Chao acel)
                                                                                                                  else let prox_incl = getInclinacao (getElemMapa pista (floor dist + 1) mapa)
                                                                                                                           incl_atual = getInclinacao (getElemMapa pista (floor dist) mapa)
                                                                                                                       in if prox_incl >= incl_atual then Jogador pista (fromIntegral(floor dist)+1) velo cola (Chao acel)
                                                                                                                                                     else let altura_final = fromIntegral (getAlturaFinal (getElemMapa pista (floor dist) mapa))
                                                                                                                                                          in Jogador pista (fromIntegral(floor dist)+1) velo cola (Ar altura_final incl_atual 0)
{-move tempo mapa (Jogador pista dist velo cola (Ar alt incl grav)) = let incl_peca_atual = getInclinacao (getElemMapa pista (floor dist) mapa)
                                                                    in if incl >= incl_peca_atual then let dist_atualizada = dist + (velo * tempo) * (cos (incl * pi / 180))
                                                                                                       in if dist_atualizada < fromIntegral (ceiling dist) then let alt_atualizada = alt + (velo * tempo) * (sin (incl * pi / 180))
                                                                                                                                                                in Jogador pista dist_atualizada velo cola (Ar alt_atualizada incl grav)
                                                                                                                                                           else let alt_atualizada = alt + ((fromIntegral(ceiling dist)) - dist) * (tan (incl * pi / 180))
                                                                                                                                                                in Jogador pista (fromIntegral(ceiling dist)) velo cola (Ar alt_atualizada incl grav)
                                                                       else let reta1 = (Cartesiano dist alt, somaVetores (Cartesiano dist alt) (Polar (velo * tempo) incl))
                                                                                reta2 = (Cartesiano (fromIntegral(floor dist)) (fromIntegral(getAlturaFinal (getElemMapa pista (floor dist - 1) mapa))), Cartesiano (fromIntegral(ceiling dist)) (fromIntegral(getAlturaFinal (getElemMapa pista (floor dist) mapa))))
                                                                                colisao_xx = getAbcissaPonto (intersecao reta1 reta2)
                                                                            in if colisao_xx > fromIntegral (ceiling dist) then let alt_atualizada = alt + ((fromIntegral(ceiling dist)) - dist) * (tan (incl * pi / 180))
                                                                                                                                in Jogador pista (fromIntegral(ceiling dist)) velo cola (Ar alt_atualizada incl grav)
                                                                                                                           else if abs (incl - incl_peca_atual) >= 45 then Jogador pista colisao_xx velo cola (Morto 1)
                                                                                                                                                                      else Jogador pista colisao_xx velo cola (Chao False)
-}
move tempo mapa (Jogador pista dist velo cola (Ar alt incl grav)) = let vetor_deslocamento = multiplicaVetor tempo (somaVetores (Polar velo incl) (Cartesiano 0 (-grav)))
                                                                        posicao_final = somaVetores (Cartesiano dist alt) vetor_deslocamento
                                                                        inicio_peca = fromIntegral (floor dist)
                                                                        reta1 = (Cartesiano dist alt, posicao_final) -- reta do deslocamento
                                                                        blocoAtual = getElemMapa pista (floor dist) mapa
                                                                        reta2 = (Cartesiano dist (getAltura blocoAtual (dist-inicio_peca)), Cartesiano (inicio_peca+1) (fromIntegral(getAlturaFinal blocoAtual))) -- reta da peça
                                                                    in if intersetam reta1 reta2 then let Cartesiano x y = intersecao reta1 reta2
                                                                                                          incl_blocoAtual = getInclinacao blocoAtual
                                                                                                      in if abs (incl - incl_blocoAtual) >= 45 then Jogador pista x 0 cola (Morto 1)
                                                                                                                                               else Jogador pista x velo cola (Chao False) 
                                                                                                 else let Cartesiano x y = posicao_final
                                                                                                      in if x < (inicio_peca+1) then Jogador pista x velo cola (Ar y incl grav)
                                                                                                                                else let new_y = alt + ((inicio_peca + 1 - dist) * (y - alt)) / (x - dist) -- regra 3 simples: em (x-dist) sobe (y-alt), logo em (iniciopeca+1-dist) sobe o que queremos que ele suba
                                                                                                                                     in Jogador pista (inicio_peca+1) velo cola (Ar new_y incl grav)


-- * Funções auxiliares da Tarefa 4

-- | Retribui o piso de uma peça do mapa
getPiso :: Peca -- ^ Peça do mapa
        -> Piso -- ^ Piso da peça correpondente
getPiso (Recta piso _) = piso
getPiso (Rampa piso _ _) = piso

-- | Retribui o atrito de uma peça do mapa
getAtrito :: Peca -- ^ Peça do mapa
          -> Double -- ^ Valor do atrito da peça correspondente
getAtrito peca = let piso = getPiso peca
                 in if piso == Terra then 0.25
                    else if piso == Relva then 0.75
                    else if piso == Lama then 1.5
                    else if piso == Boost then -0.5
                    else 3.0   -- Piso Cola

-- | Calcula a velocidade do 'Jogador', no chão, após a passagem de 't' segundos
calculaVelocidadeChao :: Double -- ^ O tempo decorrido
                      -> Double -- ^ Velocidade anterior do 'Jogador'
                      -> Bool -- ^ Indicador da aceleração do 'Jogador'
                      -> Double -- ^ Atrito da peça onde está o 'Jogador'
                      -> Double -- ^ Velocidade atualizada do 'Jogador'
calculaVelocidadeChao tempo velo acel atrito = let acel_mota = if velo < 2 && acel then 1 else 0
                                                   velo_atualizado = velo + (acel_mota - atrito * velo) * tempo
                                               in if velo_atualizado >= 0 then velo_atualizado else 0

-- | Retribui a altura final de uma peça do mapa
getAlturaFinal :: Peca -- ^ Peca do mapa
               -> Int -- ^ Altura final da peça correspondente
getAlturaFinal (Recta _ x) = x
getAlturaFinal (Rampa _ _ x) = x

-- | Retribui a abcissa de um ponto
getAbcissaPonto :: Ponto -- ^ Ponto do plano
                -> Double -- ^ Abcissa do ponto
getAbcissaPonto (Cartesiano x y) = x
getAbcissaPonto (Polar x a) = (cos (a * pi / 180)) * x

