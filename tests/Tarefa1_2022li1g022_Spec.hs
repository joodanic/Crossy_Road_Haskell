module Tarefa1_2022li1g022_Spec where

import LI12223
import Tarefa1_2022li1g022
import Test.HUnit

m1 :: Mapa
m1 = Mapa 5 []

m2 :: Mapa
m2 = (Mapa 5 [(Relva, [Arvore, Arvore, Arvore, Arvore, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m3 :: Mapa
m3 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio 1, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

m4 :: Mapa
m4 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m5 :: Mapa
m5 = (Mapa 7 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore, Nenhum]),(Estrada (-1), [Nenhum, Carro, Nenhum, Carro, Carro, Nenhum, Carro]),(Rio 1, [Tronco, Tronco, Nenhum, Tronco, Tronco,Tronco,Tronco])])

m6 :: Mapa
m6 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Carro, Nenhum, Carro, Carro]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m7 :: Mapa
m7 = (Mapa 7 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore, Nenhum]),(Estrada (-1), [Nenhum, Nenhum ,Carro, Nenhum, Carro, Carro, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco, Tronco, Tronco,Tronco,Tronco])])

m8 :: Mapa
m8 = (Mapa 6 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore, Arvore]),(Estrada 1, [Nenhum, Carro, Carro, Carro, Carro, Nenhum]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco,Nenhum])]) 

m9 :: Mapa 
m9 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (-1), [Nenhum, Nenhum,Carro, Nenhum, Carro, Carro]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m10 :: Mapa
m10 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m11 :: Mapa
m11 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m12 :: Mapa
m12 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m13 :: Mapa
m13 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Relva, [Nenhum, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m14 :: Mapa
m14 = (Mapa 5 [(Rio 1, [Tronco, Nenhum, Tronco, Nenhum, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

m15 :: Mapa
m15 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro])])

m16 :: Mapa
m16 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro])])

m17 :: Mapa
m17 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro]),(Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro,Nenhum])])

m18 :: Mapa
m18 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Relva, [Nenhum, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum])])

m19 :: Mapa
m19 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Rio (1), [Nenhum, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ test ["Mapa vazio" ~: mapaValido m1 ~=? True
                                             ,"Mapa com linha sem Nenhum" ~: mapaValido m2 ~=? False
                                             ,"Mapa com linha com pecas em Terrenos improprios" ~: mapaValido m3 ~=? False
                                             ,"Mapa valido" ~: mapaValido m4 ~=? True
                                             ,"Mapa valido com 6 Troncos numa linha sem serem seguidos e 4 Carros noutra linha sem serem seguidos" ~: mapaValido m5 ~=? True
                                             ,"Mapa com linha com largura errada" ~: mapaValido m6 ~=? False
                                             ,"Mapa com Tronco de comprimento 6" ~: mapaValido m7 ~=? False
                                             ,"Mapa com Carro de comprimento 4" ~: mapaValido m8 ~=? False 
                                             ,"Mapa com dois rios contiguos com a mesma direcao" ~: mapaValido m9 ~=? False
                                             ,"Mapa com dois rios contiguos com direcoes opostas" ~: mapaValido m10 ~=? True
                                             ,"Mapa com 4 rios contiguos" ~: mapaValido m11 ~=? True
                                             ,"Mapa com 5 rios contiguos" ~: mapaValido m12 ~=? False
                                             ,"Mapa com 5 relvas contiguas" ~: mapaValido m13 ~=? True
                                             ,"Mapa com 6 relvas contiguas" ~: mapaValido m14 ~=? False
                                             ,"Mapa com 5 estradas contiguas" ~: mapaValido m15 ~=? True
                                             ,"Mapa com 6 estradas contiguas" ~: mapaValido m16 ~=? False
                                             ,"Mapa com 6 estradas" ~: mapaValido m17 ~=? True
                                             ,"Mapa com 6 relvas" ~: mapaValido m18 ~=? True
                                             ,"Mapa com 5 rios" ~: mapaValido m19 ~=? True]
