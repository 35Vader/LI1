-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11920
import Graphics.Gloss
import Tarefa1_2019li1g011

type Posição = (Float,Float)
type Estado = [Jogador,Posição,Mapa]

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = undefined

estadoInicialJogador :: Estado
estadoInicial = (x,y)

desenhaEstado :: Estado -> Picture
desenhaEstado (x,y) = Translate x y mota 
              where 
              	mota :: Picture
              	mota = 

