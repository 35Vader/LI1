{- | Module      : Tarefa3_2019li1g011
   | Escola      : Universidade Do Minho
   | Description : Descontruir Mapas
   | Copyright   : Alexandra Dias Candeias <a89521@alunos.uminho.pt>
                   Francisco Correia Franco <a89458@alunos.uminho.pt>

= Introdução Tarefa 3:
O foco principal da nossa tarefa 3 foi tornar um mapa num conjunto de instruções retornando um grupo de bulldozers que começam a constriur o mapa referido desde o início

= Objetivos e Estratégias utilizadas:
Nesta tarefa 3 desconstruímos, quer uma pista do mapa, quer o mapa do jogo, através dos índices de pistas, 
da mesma e da próxima a ser desconstruída, respetivamente, dando um conjunto de instruções, onde no primeiro
caso dará uma lista de instruções para construir a pista, e no segundo caso, dará uma matriz de instruções para 
construir o mapa.   

= Conclusão:
Para finalizar juntamos as duas funções numa só com o propósito de tornar um mapa num conjunto de instruções.

-}
-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g011 where

import LI11920
import Tarefa2_2019li1g011

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [[[Recta Terra 0,Recta Boost 0,Recta Lama 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Lama 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]], 
           [[Recta Terra 0,Recta Lama 0,Recta Boost 0,Recta Lama 0,Recta Lama 0, Recta Boost 0, Recta Terra 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0,Rampa Lama 0 2, Recta Terra 2, Recta Boost 2, Rampa Lama 2 0]],
           [[Recta Terra 0, Rampa Terra 0 3, Recta Lama 3, Rampa Terra 3 0, Recta Boost 0],[ Recta Terra 0, Rampa Terra 0 3, Recta Lama 3, Rampa Lama 3 0, Recta Boost 0],[ Recta Terra 0, Rampa Terra 0 3, Recta Lama 3, Rampa Lama 3 0, Recta Boost 0]],
           [[Recta Terra 0, Rampa Lama 0 2, Recta Lama 2, Recta Terra 2, Rampa Terra 2 0, Recta Boost 0],[ Recta Terra 0, Rampa Terra 0 3, Recta Lama 3, Rampa Lama 3 5, Recta Boost 5, Rampa Terra 5 0],[ Recta Terra 0, Rampa Terra 0 3, Recta Lama 3, Rampa Terra 3 1, Recta Terra 1, Rampa Lama 1 0]],
           [[Recta Terra 0, Recta Boost 0, Recta Lama 0, Rampa Terra 0 4, Rampa Terra 4 0],[ Recta Terra 0, Rampa Terra 0 3, Recta Boost 3, Rampa Terra 3 0, Recta Boost 0],[ Recta Terra 0, Recta Boost 0, Recta Terra 0, Recta Lama 0, Recta Boost 0]],
           [[Recta Terra 0, Rampa Terra 0 5, Recta Lama 5, Rampa Terra 5 3, Recta Boost 3, Rampa Lama 3 0, Recta Boost 0],[ Recta Terra 0, Recta Lama 0 , Recta Boost 0, Rampa Lama 0 3, Recta Boost 3, Recta Terra 3, Rampa Terra 3 0]]]


-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi mapa = desconstroiMapa mapa 0


-- | Desconstroi uma o mapa do jogo
desconstroiMapa :: Mapa -- ^ Mapa a ser desconstruído
                -> Int -- ^ Índice da próxima pista a ser desconstruída
                -> Instrucoes -- ^ Matriz de instruções para construir o mapa
desconstroiMapa [] _ = []
desconstroiMapa ((b:bs):ps) x = (desconstroiPista bs x) ++ (desconstroiMapa ps (x+1))

-- | Desconstroi uma pista do mapa
desconstroiPista :: Pista -- ^ Pista a ser desconstruída
                 -> Int -- ^ Índice da pista a ser desconstruída
                 -> Instrucoes -- ^ Lista de instruções para construir a pista
desconstroiPista [] _ = []
desconstroiPista (Recta piso x : bs) i = Anda [i] piso : desconstroiPista bs i
desconstroiPista (Rampa piso x y : bs) i = let declive = y - x
                                           in if declive > 0 then Sobe [i] piso declive : desconstroiPista bs i else Desce [i] piso (-declive) : desconstroiPista bs i





