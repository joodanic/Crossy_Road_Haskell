{- |
Module      : Tarefa4_2022li1g022
Description : Determinar se o jogo terminou
Copyright   : João Carvalho <a94015@alunos.uminho.pt>
              Gonçalo Faria Gonçalves <a100833@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g022 where

import LI12223

-- | funçao que determina se o jogo acabou retornando True em caso de ter terminado e False no caso de nao ter terminado
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo j@(Jogador c) m@(Mapa l r)) = findLL j (converteobs1 (converteMO m) (0,0))

{- | Função que retorna uma lista de pares (Terreno,[Obstaculo]) do mapa. Esta função permite uma utilização mais fácil dos terrenos do mapa e dos seus obstaculos, como por exemplo na
 função 'converteobs'.
-}
converteMO :: Mapa -> [(Terreno,[Obstaculo])]
converteMO (Mapa l []) = []
converteMO (Mapa l ((t,o):rs)) = (t,o):converteMO (Mapa l rs)

-- | funcao que atribui coordenadas a cada uma das linhas do mapa
converteobs1 :: [(Terreno,[Obstaculo])] -> Coordenadas -> [[(Terreno,Obstaculo,Coordenadas)]]
converteobs1 [] (x,y) = []
converteobs1 (h:t) (x,y) = converteO h (x,y):converteobs1 t newC
          where newC = (x,y+1)

-- | funçao que recebe um tuplo (Terreno,[Obstaculo]) e umas Coordenadas e atribiu coordenadas a cada um dos Obstaculos de uma dada linha
converteO :: (Terreno,[Obstaculo]) -> Coordenadas -> [(Terreno,Obstaculo,Coordenadas)]
converteO (t,[]) (x,y) = []
converteO (t,(h:rs)) (x,y) = (t,h,(x,y)):converteO (t,rs) newC
        where newC = (x+1,y)

-- | funçao que verifica se os jogador se encontra numa posicao valida no mapa retornando False caso se encontre numa posiçao valida e True caso se encontre fora do mapa ou numa posiçao invalida
findLL :: Jogador -> [[(Terreno,Obstaculo,Coordenadas)]] -> Bool
findLL (Jogador (x,y)) [] = True
findLL j@(Jogador (x,y)) (l:ls) = if (find j l == 0) then findLL j ls else if (find j l == 2) then False else True

{- | funçao que verifica se um jogador se encontra numa posiçao valida numa dada linha do mapa, retornando 0 caso o Jogador não se encontre nesta linha,
  retornando 1 caso este se encontre debaixo de um Carro ou debaixo de Agua, e retornando 2 caso este se encontre numa outra posição valida
-}
find :: Jogador -> [(Terreno,Obstaculo,Coordenadas)] -> Int
find (Jogador (x,y)) [] = 0
find j@(Jogador (x,y)) ((t,h,c):rs) |((x,y) == c && h == Nenhum && t == Rio (checkVelocidade t)) = 1
                                    |((x,y) == c && h == Carro && t == Estrada (checkVelocidade t)) = 1
                                    |((x,y)) == c = 2
                                    |otherwise = find j rs

-- | retira a velocidade de um Rio e de uma Estrada
checkVelocidade :: Terreno -> Velocidade
checkVelocidade (Rio v) = v
checkVelocidade (Estrada v) = v

