module Tarefa4_2022li1g022_Spec where

import LI12223
import Tarefa4_2022li1g022
import Test.HUnit

jogo1 :: Jogo
jogo1 = Jogo j1 m1

j1 :: Jogador
j1 = Jogador (3,1)

m1 :: Mapa
m1 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

jogo2 :: Jogo
jogo2 = Jogo j2 m2

j2 :: Jogador
j2 = Jogador (2,1)

m2 :: Mapa
m2 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

jogo3 :: Jogo
jogo3 = Jogo j3 m3

j3 :: Jogador
j3 = Jogador (5,0)

m3 :: Mapa
m3 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])]) 

jogo4 :: Jogo
jogo4 = Jogo j4 m4

j4 :: Jogador
j4 = Jogador (3,1)

m4 :: Mapa
m4 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (-1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

jogo5 :: Jogo
jogo5 = Jogo j5 m4

j5 :: Jogador
j5 = Jogador (1,0)

jogo6 :: Jogo
jogo6 = Jogo j6 m3

j6 :: Jogador
j6 = Jogador (0,1)

jogo7 :: Jogo
jogo7 = Jogo j7 m4

j7 :: Jogador
j7 = Jogador (0,3)

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Jogador debaixo de Carro" ~: jogoTerminou jogo1 ~=? True
                                             ,"Jogador na Agua" ~: jogoTerminou jogo2 ~=? True
                                             ,"Jogador fora do mapa 1" ~: jogoTerminou jogo3 ~=? True
                                             ,"Jogador em cima de Tronco" ~: jogoTerminou jogo4 ~=? False
                                             ,"Jogador na relva" ~: jogoTerminou jogo5 ~=? False
                                             ,"Jogador na estrada" ~: jogoTerminou jogo6 ~=? False
                                             ,"Jogador fora do mapa 2" ~: jogoTerminou jogo7 ~=? True]
