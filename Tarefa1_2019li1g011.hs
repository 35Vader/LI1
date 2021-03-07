{- | Module      : Tarefa1_2019li1g011
   | Escola      : Universidade Do Minho
   | Description : Gerar Mapas
   | Copyright   : Alexandra Dias Candeias <a89521@alunos.uminho.pt>
                   Francisco Correia Franco <a89458@alunos.uminho.pt>

= Introdução Tarefa 1:
Esta tarefa tem como principal objetivo implementar uma ferramenta capaz de gerar mapas.

= Objetivos e Estratégias utilizadas:
Começamos por descodificar o tipo e o piso da peça atual para depois gerar e retribuir uma peça. Decidimos então gerar uma determinada pista e consequentemente um mapa através das pistas geradas anteriormente.
De seguida resolvemos transformar uma lista numa matriz mas limitamos cada linha a um determinado número de elementos. Com isto replicamos uma peça um determinado número de vezes com a finalidade de obter um determinado mapa.
Por fim, geramos o mapa do jogo, dependendo do número e comprimento das pistas e da semente geradora do mapa. 

= Conclusão:
Em suma, pretendemos gerar aleatoriamente um mapa de jogo, resultante de outras criações aleatórias para cada componente do mapa.

-}

-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g011 where
  

import Tarefa0_2019li1g011
import LI11920
import System.Random
import Data.List

-- * Testes

-- | Testes unitários da Tarefa 1.


-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).
testesT1 :: [(Int,Int,Int)]
testesT1 = [(2,5,1),(3,9,2),(1,4,1),(1,1,1),(3,7,2),(1,8,2),(3,3,3),(8,5,3),(2,2,2),(3,3,3),(4,4,4),(2,6,3),(1,4,3),(3,7,3)]



-- * Funções pré-definidas da Tarefa 1.

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = take n (randomRs (0,9) (mkStdGen seed))

-- * Funções principais da Tarefa 1.

-- | Gera o mapa do jogo
gera :: Int -- ^ Números de pistas a ser gerados
     -> Int -- ^ Comprimento das pistas
     -> Int -- ^ Semente geradora do mapa
     -> Mapa -- ^ Mapa do jogo
gera npistas comprimento semente = let tamanho_pista = (comprimento-1) * 2
                                       lista_geradores = geraAleatorios (npistas * tamanho_pista) semente
                                       matriz_geradores = listaParaMatriz tamanho_pista lista_geradores
                                   in if matriz_geradores == [] then replica npistas (Recta Terra 0)
                                      else geraMapa matriz_geradores


-- | Replica uma peça 'npistas' número de vezes
replica :: Int -- ^ Número de replicações
        -> Peca -- ^ Peça a replicar
        -> Mapa -- ^ Mapa com as replicações 
replica 0 _ = []
replica x peca = [peca] : replica (x-1) peca


-- | Tranforma uma lista numa matriz em que cada linha tem um número escolhido de elementos
listaParaMatriz :: Int -- ^ Tamanho de cada linha da matriz
                -> [Int] -- ^ Array a ser transformado
                -> [[Int]] -- ^ Matriz final
listaParaMatriz _ [] = []
listaParaMatriz size array = take size array : listaParaMatriz size (drop size array)


-- | Gera um mapa
geraMapa :: [[Int]] -- ^ Matriz em que cada linha corresponde aos inteiros geradores duma única pista
         -> Mapa -- ^ Mapa com as peças correspondentes da matriz de inteiros
geraMapa [] = []
geraMapa (h:t) = (Recta Terra 0 : geraPista h (Recta Terra 0)) : geraMapa t


-- | Gera uma pista
geraPista :: [Int] -- ^ Lista de inteiros geradores das peças duma única pista
          -> Peca -- ^ Peça anterior
          -> Pista -- ^ Pista com as peças correspondentes da lista de inteiros
geraPista [] _ = []
geraPista (piso:tipo:t) peca_ant = peca : geraPista t peca
                                 where peca = geraPeca piso tipo peca_ant


-- | Recebe os inteiros geradores de uma única peça e a peça anterior e retribui a peça atual
geraPeca :: Int -- ^ Inteiro gerador do piso da peça atual
         -> Int -- ^ Inteiro gerador do tipo da peça atual
         -> Peca -- ^ Peça anterior
         -> Peca -- ^ Peça atual
geraPeca piso_int tipo_int peca_ant = let piso = descodificaPiso piso_int peca_ant
                                      in descodificaTipo piso tipo_int peca_ant                                            


-- | Descodifica o piso da peça atual
descodificaPiso :: Int -- ^ Inteiro que gera o piso atual
                -> Peca -- ^ Peca anterior
                -> Piso -- ^ Piso atual
descodificaPiso piso_int (Rampa piso _ _) = if piso_int == 0 || piso_int == 1 then Terra
                                            else if piso_int == 2 || piso_int == 3 then Relva
                                            else if piso_int == 4 then Lama
                                            else if piso_int == 5 then Boost
                                            else piso 
descodificaPiso piso_int (Recta piso _ ) = if piso_int == 0 || piso_int == 1 then Terra
                                           else if piso_int == 2 || piso_int == 3 then Relva
                                           else if piso_int == 4 then Lama
                                           else if piso_int == 5 then Boost
                                           else piso

-- | Descodifica o tipo da peça atual 
descodificaTipo :: Piso -- ^ Piso da peça atual
                -> Int -- ^ Inteiro que gera o tipo da peça atual
                -> Peca -- ^ Peça anterior
                -> Peca -- ^ Peça atual
descodificaTipo piso tipo_int (Rampa _ _ x) = if tipo_int == 0 || tipo_int == 1 then Rampa piso x (x+tipo_int+1)
                                              else if x == 0 || (tipo_int >= 6 && tipo_int <= 9) then Recta piso x
                                              else if tipo_int >= 2 && tipo_int <= 5 && x-tipo_int+1 >= 0 then Rampa piso x (x-tipo_int+1)
                                              else Rampa piso x 0
descodificaTipo piso tipo_int (Recta _ x) = if tipo_int == 0 || tipo_int == 1 then Rampa piso x (x+tipo_int+1)
                                            else if x == 0 || (tipo_int >= 6 && tipo_int <= 9) then Recta piso x
                                            else if tipo_int >= 2 && tipo_int <= 5 && x-tipo_int+1 >= 0 then Rampa piso x (x-tipo_int+1)
                                            else Rampa piso x 0


