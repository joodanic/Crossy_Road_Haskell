module Tarefa2_2022li1g022_Spec where

import LI12223
import Tarefa2_2022li1g022
import Test.HUnit

m1 :: Mapa
m1 = Mapa 5 []

m2 :: Mapa
m2 = (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m3 :: Mapa
m3 = (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro])])

m4 :: Mapa
m4 = (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Estrada (-1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

m5 :: Mapa
m5 = (Mapa 5 [(Rio 1, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

m6 :: Mapa
m6 = (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Rio 1, [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio 1, [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco])])

m7 :: Mapa
m7 = (Mapa 5 [(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro])])

m8 :: Mapa
m8 = (Mapa 5 [(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro]),(Rio (-1), [Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore]),(Rio 1,[Tronco, Nenhum, Nenhum, Tronco, Tronco]),(Relva, [Arvore, Nenhum, Nenhum, Arvore, Arvore])])

m9 :: Mapa
m9 = (Mapa 5 [(Estrada (1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro]),(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore])])
 
m10 :: Mapa
m10 = (Mapa 5 [(Relva, [Arvore, Nenhum, Arvore, Nenhum, Arvore]),(Estrada (1), [Nenhum, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada (-1), [Carro, Nenhum, Nenhum, Carro, Carro]),(Estrada 1, [Carro, Nenhum, Nenhum, Carro, Carro]),(Rio 1, [Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Estrada 1,[Carro,Nenhum,Nenhum,Carro,Nenhum])])

o1 :: (Terreno,[Obstaculo])
o1 = (Rio 1,[])

o2 :: (Terreno,[Obstaculo])
o2 = (Estrada 1,[])

o3 :: (Terreno,[Obstaculo])
o3 = (Relva,[])

o4 :: (Terreno,[Obstaculo])
o4 = (Estrada 1,[Nenhum,Carro,Carro,Carro,Nenhum])

o5 :: (Terreno,[Obstaculo])
o5 = (Estrada 1,[Nenhum,Carro,Carro,Carro])

o6 :: (Terreno,[Obstaculo])
o6 = (Rio 1,[Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum])

o7 :: (Terreno,[Obstaculo])
o7 = (Rio 1,[Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco])

o8 :: (Terreno,[Obstaculo])
o8 = (Rio 1,[Tronco,Nenhum,Tronco,Tronco])

o9 :: (Terreno,[Obstaculo])
o9 = (Relva,[Arvore,Arvore,Arvore,Arvore])

o10 :: (Terreno,[Obstaculo])
o10 = (Relva,[Arvore,Nenhum,Arvore])

o11 :: (Terreno,[Obstaculo])
o11 = (Estrada 1,[Carro,Carro,Nenhum,Carro]) 

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test ["Terrenos validos lista vazia" ~: proximosTerrenosValidos m1 ~=? [Rio 1, Estrada 1, Relva]
                                              ,"Terrenos validos mapa que acaba em rio" ~: proximosTerrenosValidos m2 ~=? [Rio (-1), Estrada 1,Relva]
                                              ,"Terrenos validos de mapa que acaba em estrada" ~: proximosTerrenosValidos m3 ~=? [Rio 1, Estrada (-1), Relva]
                                              ,"Terrenos validos de mapa que acaba em relva" ~: proximosTerrenosValidos m4 ~=? [Rio 1, Estrada 1, Relva]
                                              ,"Terrenos validos de mapa com 4 rios contiguos" ~: proximosTerrenosValidos m5 ~=? [Estrada 1,Relva]
                                              ,"Terrenos validos de mapa com 4 rios" ~: proximosTerrenosValidos m6 ~=? [Rio 1,Estrada 1,Relva]
                                              ,"Terrenos validos de mapa com 5 relvas contiguas" ~: proximosTerrenosValidos m7 ~=? [Rio 1,Estrada (-1)]
                                              ,"Terrenos validos de mapa com 5 relvas" ~: proximosTerrenosValidos m8 ~=? [Rio 1,Estrada 1,Relva]
                                              ,"Terrenos validos de mapa com 5 estradas contiguas" ~: proximosTerrenosValidos m9 ~=? [Rio 1,Relva]
                                              ,"Terrenos validos de mapa com 6 estradas" ~: proximosTerrenosValidos m10 ~=? [Rio 1, Estrada (-1),Relva]                                             
                                              ,"Obstaculos validos de rio sem obstaculos" ~: proximosObstaculosValidos 5 o1 ~=? [Tronco,Nenhum]
                                              ,"Obstaculos validos de estrada sem obstaculos" ~: proximosObstaculosValidos 5 o2 ~=? [Nenhum,Carro]
                                              ,"Obstaculos validos de relva sem obstaculos" ~: proximosObstaculosValidos 5 o3 ~=? [Nenhum,Arvore]
                                              ,"Obstaculos validos de estrada com largura 5" ~: proximosObstaculosValidos 5 o4 ~=? []
                                              ,"Obstaculos validos de estrada que termina em Nenhum" ~: proximosObstaculosValidos 6 o4 ~=? [Nenhum,Carro]
                                              ,"Obstaculos validos de estrada que termina em carro com 3 unidades" ~: proximosObstaculosValidos 5 o5 ~=? [Nenhum]
                                              ,"Obstaculos validos de rio que termina em Nenhum com tronco de 5 unidades" ~: proximosObstaculosValidos 8 o6 ~=? [Nenhum,Tronco]
                                              ,"Obstaculos validos de rio que termina em Tronco com 5 unidades" ~: proximosObstaculosValidos 7 o7 ~=? [Nenhum]
                                              ,"Obstaculos validos de rio" ~: proximosObstaculosValidos 5 o8 ~=? [Nenhum,Tronco]
                                              ,"Obstaculos validos de relva com apenas Arvore e com apenas hipotese de adicionar mais um Obstaculo" ~: proximosObstaculosValidos 5 o9 ~=? [Nenhum]
                                              ,"Obstaculos validos de relva com apenas Arvore e com hipotese de adicionar 2 ou mais elementos" ~: proximosObstaculosValidos 6 o9 ~=? [Nenhum,Arvore]
                                              ,"Obstaculos validos de relva com Nenhum e Arvore" ~: proximosObstaculosValidos 4 o10 ~=? [Nenhum,Arvore]
                                              ,"Obstaculos validos de estrada" ~: proximosObstaculosValidos 5 o11 ~=? [Nenhum,Carro]
                                            ]
