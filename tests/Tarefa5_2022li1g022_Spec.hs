module Tarefa5_2022li1g022_Spec where

import LI12223
import Tarefa5_2022li1g022
import Test.HUnit

n :: Int
n = 10

m1 :: Mapa
m1 = (Mapa 3 [(Estrada 1,[Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum])])

jogador1 :: Jogador
jogador1 = (Jogador (1,1))

jog1 :: Jogo
jog1 = (Jogo jogador1 m1)

jog2 :: Jogo
jog2 = (Jogo (Jogador (1,2)) (Mapa 3 [(Estrada 1,[Nenhum,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum])]))

jog3 :: Jogo
jog3 = (Jogo (Jogador (1,2)) (Mapa 3 [(Rio (-1),[Nenhum,Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Nenhum])]))

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test ["Desliza jogo mapa 1" ~: deslizaJogo n jog1 ~=? jog2
                                              ,"Desliza jogo mapa 1, com inteiro diferente" ~: deslizaJogo (n+11) jog1 ~=? jog3]


