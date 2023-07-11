module Tarefa3_2022li1g022_Spec where

import LI12223
import Tarefa3_2022li1g022
import Test.HUnit

par :: Jogada
par = Parado

md :: Jogada
md = (Move Direita)

me :: Jogada
me = (Move Esquerda)

mc :: Jogada
mc = (Move Cima)

mb :: Jogada
mb = (Move Baixo)

m1 :: Mapa
m1 = (Mapa 5 [(Estrada (2), [Nenhum, Carro, Carro, Nenhum, Carro]),
              (Rio 2, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),
              (Estrada (-2), [Carro, Nenhum, Nenhum, Carro, Carro]),
              (Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

m1s :: Mapa
m1s = (Mapa 5 [(Estrada (2), [Nenhum,Carro,Nenhum,Carro,Carro]),(Rio 2, [Nenhum,Tronco,Tronco,Nenhum,Tronco]),(Estrada (-2), [Nenhum, Carro, Carro,Carro,Nenhum]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

jogo1 :: Jogo
jogo1 = Jogo j1 m1

j1 :: Jogador
j1 = Jogador (1,3)

jogo2 :: Jogo
jogo2 = Jogo j2 m1

j2 :: Jogador
j2 = Jogador (0,0)

jogo3 :: Jogo
jogo3 = Jogo j3 m1

j3 :: Jogador 
j3 = Jogador (1,1)

jogo4 :: Jogo
jogo4 = Jogo j4 m1

j4 :: Jogador
j4 = Jogador (1,3)

jogo5 :: Jogo
jogo5 = Jogo j5 m1

j5 :: Jogador 
j5 = Jogador (2,3)

jogo6 :: Jogo
jogo6 = Jogo j6 m1

j6 :: Jogador 
j6 = Jogador (2,0)

jogo7 :: Jogo
jogo7 = Jogo j7 m1

j7 :: Jogador
j7 = Jogador (3,1)

jogo8 :: Jogo
jogo8 = Jogo j8 m1

j8 :: Jogador
j8 = Jogador (0,2)

jogo9 :: Jogo
jogo9 = Jogo j9 m1

j9 :: Jogador
j9 = Jogador (2,1) 

jogo10 :: Jogo
jogo10 = Jogo j10 m1

j10 :: Jogador
j10 = Jogador (4,1)

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Mover o jogador para a direita quando este pode" ~: animaJogo jogo1 md ~=? (Jogo (Jogador (2,3)) m1s)
                                             ,"Mover o jogador para a direita quando este nao pode" ~: animaJogo jogo2 md ~=? (Jogo (Jogador (0,0)) m1s)
                                             ,"Mover o jogador para fora do mapa" ~: animaJogo jogo2 me ~=? (Jogo (Jogador (0,0)) m1s)
                                             ,"Mover o jogador para a esquerda quando este pode" ~: animaJogo jogo3 me ~=? (Jogo (Jogador (0,1)) m1s)
                                             ,"Mover o jogador para a esquerda quando este nao pode" ~: animaJogo jogo4 me ~=? (Jogo (Jogador (1,3)) m1s)
                                             ,"Mover para baixo quando esta no limite do mapa" ~: animaJogo jogo5 mb ~=? (Jogo (Jogador (2,3)) m1s)
                                             ,"Mover para cima quando esta no limite do mapa" ~: animaJogo jogo6 mc ~=? (Jogo (Jogador (2,0)) m1s)
                                             ,"Mover para baixo quando este pode" ~: animaJogo jogo6 mb ~=? (Jogo (Jogador (2,1)) m1s)
                                             ,"Mover para baixo quando este nao pode" ~: animaJogo jogo7 mb ~=? (Jogo (Jogador (3,1)) m1s)
                                             ,"Mover para cima quando este pode" ~: animaJogo jogo8 mc ~=? (Jogo (Jogador (0,1)) m1s)
                                             ,"Movimento parado" ~: animaJogo jogo8 par ~=? (Jogo (Jogador (0,2)) m1s)
                                             ,"Movimento parado em cima de Tronco" ~: animaJogo jogo9 par ~=? (Jogo (Jogador (4,1)) m1s)
                                             ,"Movimento parado em cima de Tronco que é movido para fora do mapa" ~: animaJogo jogo10 par ~=? (Jogo (Jogador (6,1)) m1s)]
