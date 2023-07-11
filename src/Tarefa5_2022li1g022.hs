{- |
Module      : Tarefa5_2022li1g022
Description : Validação de um mapa
Copyright   : João Carvalho <a94015@alunos.uminho.pt>
              Gonçalo Faria Gonçalves <a100833@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g022 where

import LI12223
import Tarefa2_2022li1g022

-- | função responsável por acrescentar uma linha ao fim do mapa e retirar a 1ª linha
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo n (Jogo (Jogador (x,y)) (Mapa l m)) = (Jogo (Jogador (x,y+1)) (estendeMapa newM n))
        where newM = (Mapa l (removeULinha m))
-- | função que remove a cabeça da lista
removeULinha :: [a] -> [a]
removeULinha [] = []
removeULinha l1@(l:ls) = reverse (removeLinha (reverse l1))
-- | função que dá a cauda de uma lista
removeLinha :: [a] -> [a]
removeLinha [] = []
removeLinha (l:ls) = ls



