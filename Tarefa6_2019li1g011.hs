{- | Module      : Tarefa6_2019li1g011
   | Escola      : Universidade Do Minho
   | Description : Implementar um Robô
   | Copyright   : Alexandra Dias Candeias <a89521@alunos.uminho.pt>
                           Francisco Correia Franco <a89458@alunos.uminho.pt>

= Introdução Tarefa 6:
Foi nesta tarefa que o nosso grupo desenvolveu e realizou o bot do jogo “Excite bike”.

= Objetivos e Estratégias utilizadas:
Como pedia o guião, criamos um bot em que o objetivo é verificar se o próximo piso da pista de cima, bem como o da pista de baixo, tem menos atrito que o proximo piso da pista onde se encontra. 
Se por acaso essa condição se verificar, o bot deslocar-se-à para essa mesma pista, se não, ele mantem-se na mesma pista e acelera.
Mas se por ventura as duas outras pistas tiverem um atrito menor que a pista onde ele se encontra, ele irá movimentar-se para a que apresenta um menor atrito.

= Conclusão:
Para resumir, pretendemos criar um bot que se movimenta no mapa, ao longo das pistas, de acordo com a quantidade de atrito que cada piso apresentava,
com a finalidade de chegar mais rápido ao final do mapa, ao evitar ao máximo possível o atrito do jogo, uma vez que o atrito diminui a velocidade dos
jogadores ao longo do jogo.

-}
-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g011 where

import LI11920
import Tarefa4_2019li1g011
import Tarefa2_2019li1g011
-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot j (Estado m js) = fazJogada (js!!j) m

fazJogada :: Jogador -> Mapa -> Maybe Jogada
fazJogada (Jogador _ _ _ _ (Morto _)) _ = Nothing
fazJogada (Jogador pista pos _ _ (Ar _ inc _)) m|inc < -15 = Just(Movimenta D)
                                                |inc > 15 = Just(Movimenta E)
fazJogada j@(Jogador pista _ _ _ (Chao _)) m  |(comparaPista j m) == pista = Just(Acelera)
                                              |(comparaPista j m) == pista+1 = Just(Movimenta B)
                                              |(comparaPista j m) == pista-1 = Just(Movimenta C)

comparaPista :: Jogador -> Mapa -> Int --Retorna a pista para qual deve ir o bot
comparaPista (Jogador pista pos _ _ _) m |pista == 0 = if getAtrito(getElemMapa (pista+1) (floor pos+1) m) < valor then pista + 1 else pista
                                         |pista == (length m)-1 = if getAtrito(getElemMapa (pista-1) (floor pos-1) m) < valor then pista - 1 else pista
                                         |getAtrito(getElemMapa (pista+1) (floor pos+1) m)>getAtrito(getElemMapa (pista-1) (floor pos-1) m) = if getAtrito(getElemMapa (pista-1) (floor pos-1) m)<valor then pista -1 else pista
                                         |otherwise = if getAtrito(getElemMapa (pista+1) (floor pos+1) m)< valor then pista +1 else pista
                                         where 
                                            valor = getAtrito(getElemMapa pista (floor pos) m)



