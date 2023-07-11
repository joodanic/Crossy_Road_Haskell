{- |
Module      : Tarefa2_2022li1g022
Description : Geração contínua de um mapa
Copyright   : João Carvalho <a94015@alunos.uminho.pt>
              Gonçalo Faria Gonçalves <a100833@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g022 where

import LI12223
import Tarefa1_2022li1g022

{- | Funçao que acrescenta uma linha no topo do mapa, com o auxílio das funções: 'chooseObstaculoL', 'proximosObstaculosValidos',
'chooseT', 'proximosTerrenosValidos'
-}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa m@(Mapa l rs) n = (Mapa l (((t),(chooseObstaculoL n l (tamanho(proximosObstaculosValidos l (t,[]))) t (proximosObstaculosValidos l (t,[])))):rs)) 
            where t = (chooseT n (tamanho (proximosTerrenosValidos m)) (proximosTerrenosValidos m))

{- | Funçao que escolhe l obstaculos de uma lista de obstaculos com o auxílio das funções 'switch', 'chooseObstaculo' e 'chooseObstaculoL',
'verifica' e 'dividir', de tal forma que, ao mapa já existente, há-de acrescentar uma lista de __Obstáculos__ válida, de forma aleatória.
É escolhida uma lista de __Obstáculos__ ao acaso pela 'chooseObstaculo' e pela recursividade, onde em cada iteração, se tira 1 ao valor responsáve
pelo término da lista. Se essa lista for válida, então é acrescentada, caso contrário, a função @chooseObstaculoL@ é chamada a tentar achar uma lista
válida, mas passando um valor aleatório conseguido pela função 'dividir'. 
-}
chooseObstaculoL :: Int -> Int -> Int -> Terreno -> [Obstaculo] -> [Obstaculo]
chooseObstaculoL 0 l t (Rio v) os = switch (Rio v) l
chooseObstaculoL 0 l t (Estrada v) os = switch (Estrada v) l
chooseObstaculoL 0 l t (Relva) os = switch (Relva) l
chooseObstaculoL n 0 t tr os = []
chooseObstaculoL n l t tr os = if verifica (obs) == True then obs else (chooseObstaculoL (n-1) l t tr os)
                 where obs = (chooseObstaculo n t os):chooseObstaculoL new (l-1) t tr os
                       new=dividir n

{- | Esta função recebe uma lista de __Obstáculos__ criada aleatoriamente pela 'chooseObstaculo' e verifica se pode
ser acrescentada ao mapa ou não, com assistência da @verificaTronco@, @pegaTronco@, @verificaCarro@, @pegaCarro@ e @verificaNenhum@.
-}
verifica :: [Obstaculo] -> Bool
verifica [] = True
verifica l = verificaTronco (pegaTronco l) && verificaCarro (pegaCarro l) && verificaNenhum l

{- | Esta função é a função responsável pela atribuição aleatória dos __Obstáculos__ que vão ser atribuídos à nova lista de __Obstáculos__
no mapa. Esta função será sempre possível, uma vez que vai ser chamada várias vezes e ao dividirmos o número e lhe retirarmos 1, eventualmente,
o número a ser passado seria <= 0, mas isso não é problema, uma vez que a função, nesse caso, retorna o número 100 de modo a manter a função sempre
funcional, independentemente das vezes que é chamada.
-}

dividir :: Int -> Int
dividir n1 = if n1<=0 then 100 else (div n1 2)-1

{- | Funçao que escolhe uma obstaculo de entre uma lista de __Obstaculos__. Este @n@ é dado pelo número de linhas a adicionar na 'estendeMapa' e
este @t@ é dado através da largura do mapa e esta lista de __Obstaculos__ é dada pela 'proximosObstaculosValidos' em relação ao __Terreno__ escolhido
aleatoriamente pela 'chooseT'. Com isto, ao usar a função @!!@, vai-se buscar um __Obstaculo__ à lista na posição correspondente ao
resto da divisão de @n@ por @t@, assegurando a aleatoriedade.
-} 
chooseObstaculo :: Int -> Int -> [Obstaculo] -> Obstaculo
chooseObstaculo n t (h:hs) = ((h:hs) !! (mod n t))

{- | Esta função utiliza a função 'putInmid' e a 'switchN' de modo a adicionar o __Obstáculo__ correspondente ao __Terreno__ em questão, intercalando
um __Nenhum__ e um __Obstaculo__ válido.
-}
switch :: Terreno -> Int -> [Obstaculo]
switch t 0 = []
switch (Rio v) l = (take l (putInmid (Tronco) (switchN (Rio v) l)))
switch (Estrada v) l = (take l (putInmid (Carro) (switchN (Estrada v) l)))
switch (Relva) l = (take l (putInmid (Arvore) (switchN (Relva) l)))

{- | Esta função faz uma lista de __Nenhum__ de tamanho @n@, onde n é igual à largura do mapa.
-}
switchN :: Terreno -> Int -> [Obstaculo]
switchN t 0 = []
switchN t n = Nenhum:switchN t (n-1)

{- | Esta função intercala cada elemento de uma lista com um elemento dado. Este elemento intercalado será o __Obstaculo__ válido em cada __Terreno__
utilizado na 'switch'.
-}
putInmid :: a -> [a] -> [a]
putInmid _ [] = []
putInmid y [x] = [x]
putInmid y (x:xs) = x:y:putInmid y xs

-- | Esta função escolhe um __Terreno__ entre uma lista de __Terrenos__, semelhante à 'chooseObstaculo'.
chooseT :: Int -> Int -> [Terreno] -> Terreno
chooseT n l (h:hs) = ((h:hs) !! (mod n l))

{- | funçao que recebe um mapa e retorna uma lista de possiveis __Terrenos__ seguintes.
Nos 1º e 2º casos, verifica-se se o último __Terreno__ do mapa é um __Rio__. Se for esse o caso, passa-se à verificação se o mapa já tem 4 rios, com o auxílio da 'verificaRiosI' e @pegaRios@. Se tiver,
os únicos __Terrenos__ possíveis são __Relva__ e __Estrada__, caso contrário, seria __Relva__, __Estrada__ e __Rio__.
Nos 3º e 4º casos, verifica-se se o último __Terreno__ do mapa é uma __Estrada__. Se for esse o caso, passa-se à verificação se o mapa já tem
5 __Estradas__ contíguas com o auxílio das funções 'verificaERI' e da @pegaEst@. Se existirem 5 __Estradas__ seguidas, então, os únicos __Terrenos__
possíveis serão __Rio__ e __Relva__, caso contrário, seria __Relva__, __Estrada__ e __Rio__.
Nos 5º e 6º casos, verifica-se se o último __Terreno__ do mapa é uma __Relva__. Se for esse o caso, passa-se à verificação se o mapa já tem 5
__Relvas__ contíguas com o auxílio das funções 'verificaERI' e da @pegaRel@. Se for esse o caso, os únicos __Terrenos__ possíveis serão
__Rio__ e __Estrada__, caso contrário, serão __Rio__, __Estrada__ e __Relva__.
-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 1, Estrada 1, Relva]
proximosTerrenosValidos m@(Mapa l ((t,(o:os)):rs)) |(head (converteMT m) == Rio (checkVelocidade (head (converteMT m)))) && ((verificaRiosI ((pegaRios (converteMT m)))) == False) = [(Estrada (velocidadeE(last(converteMT m)))),Relva] 
                                                   |(head (converteMT m) == Rio (checkVelocidade (head (converteMT m)))) && (verificaRiosI ((pegaRios (converteMT m))))= [(Rio (velocidadeR(last(converteMT m)))), (Estrada (velocidadeE(last(converteMT m)))), Relva]
                                                   |(head (converteMT m) == Estrada (checkVelocidade (head (converteMT m)))) && ((verificaERI ((pegaEst (converteMT m)))) == False) = [(Rio (velocidadeR(last(converteMT m)))),Relva]
                                                   |(head (converteMT m) == Estrada (checkVelocidade (head (converteMT m)))) && (verificaERI ((pegaEst (converteMT m)))) = [(Rio (velocidadeR(last(converteMT m)))),(Estrada (velocidadeE(last(converteMT m)))),Relva]
                                                   |(head (converteMT m) == Relva) && ((verificaERI ((pegaRel (converteMT m)))) == False) = [(Rio (velocidadeR(last(converteMT m)))),(Estrada (velocidadeE(last(converteMT m))))]
                                                   |(head (converteMT m) == Relva) && (verificaERI ((pegaRel (converteMT m)))) = [(Rio (velocidadeR(last(converteMT m)))),(Estrada (velocidadeE(last(converteMT m)))),Relva]

{- | Funçao que recebe um par de __Terreno__ e lista de __Obstaculos__ e retorna uma lista de possiveis __Obstaculos__ seguintes.
Nos 3 casos singulares, define-se que, independentemente do __Terreno__, __Nenhum__ fará sempre sentido estar como possível __Obstáculo__.
No 4 caso, é o caso de paragem para quando o tamanho da lista de __Obstáculos__ se igual ao tamanho do mapa.
No 5 caso, caso o __Terreno__ seja __Rio__ e ainda não haja nenhum  __Nenhum__ e ainda falte ser preenchido um __Obstáculo__, esse __Obstáculo__ apenas poderá ser __Nenhum__,
uma vez que tem que existir um __Nenhum__ em cada __Terreno__ de modo a possibilitar a jogabilidade do __Mapa__.
No caso 6,  caso o __Terreno__ seja __Rio__ e o último __Obstáculo__ seja __Nenhum__, o próximo __Obstáculo__ pode ser tanto __Nenhum__ como __Tronco__.
No caso 7, caso o __Terreno__ seja __Rio__ e o último __Obstáculo__ seja __Tronco__ e já haja 5 __Tronco__ seguidos, então o próximo __Obstáculo__ só pode ser o __Nenhum__.
No caso 8, caso o __Terreno__ seja __Rio__ e ainda não tenha 5 __Troncos__ seguidos, o próximo __Obstáculo__ pode ser tanto __Nenhum__ como __Tronco__.
No caso 9, caso o __Terreno__ seja __Estrada__ e ainda não haja nenhum __Obstáculo__ __Nenhum__, então o próximo terá que ser __Nenhum__.
No caso 10, caso o __Terreno__ seja __Estrada__ e o último __Obstáculo__ seja __Nenhum__, o próximo pode ser tanto __Carro__ como __Nenhum__.
No caso 11, caso o __Terreno__ seja __Estrada__ e os últimos __Obstáculos__ sejam __Carro__, o próximo __Obstáculo terá que ser __Nenhum__.
No caso 12, caso o __Terreno__ seja __Estrada__ e ainda não haja 5 __Carros__ seguidos, o próximo __Obstáculo__ pode ser tanto __Nenhum__ como __Carro__.
No caso 13, caso o __Terreno__ seja __Relva__ e seja o último __Obstáculo__ possível e ainda não haja nenhum __Nenhum__, só pode ser o __Nenhum__.
No caso 14, caso o __Terreno__ seja __Relva__ e já haja __Nenhum__ na lista de __Obstáculos__, então tanto pode ser __Nenhum__ como __Arvore__.

-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos _ (Rio _ ,[]) = [Tronco,Nenhum]
proximosObstaculosValidos _ (Estrada _ ,[]) = [Nenhum,Carro]
proximosObstaculosValidos _ (Relva,[]) = [Nenhum,Arvore]
proximosObstaculosValidos l (t,os) |tamanho (os) == l = []
                                   |(t == Rio (checkVelocidade t)) && ((verificaNenhum os) == False) && (length(os) == (l-1)) = [Nenhum]
                                   |((t==Rio (checkVelocidade t)) && (last (os) == Nenhum)) = [Nenhum,Tronco]
                                   |((t==Rio (checkVelocidade t)) && (last (os) == Tronco) && ((verificaTroncoI (last (pegaTronco (os)))) == False)) = [Nenhum]
                                   |((t == Rio (checkVelocidade t)) && ((verificaTroncoI (last (pegaTronco (os)))))) = [Nenhum,Tronco]
                                   |(t == Estrada (checkVelocidade t)) && ((verificaNenhum os) == False) && (length(os) == (l-1))= [Nenhum]
                                   |((t == Estrada (checkVelocidade t)) && (last(os) == Nenhum)) = [Nenhum,Carro]
                                   |((t == Estrada (checkVelocidade t)) && (last(os) == Carro) && ((verificaCarroI (last (pegaCarro (os)))) == False)) = [Nenhum]
                                   |((t == Estrada (checkVelocidade t)) && ((verificaCarroI (last (pegaCarro (os)))))) = [Nenhum,Carro]
                                   |(t == Relva) && ((verificaNenhum os) == False) && (length(os) == (l-1))= [Nenhum]
                                   |(t == Relva) && (verificaNenhum os) = [Nenhum,Arvore]
                                   |(t == Relva) = [Nenhum,Arvore]


-- | Funçao que verifica que se ja existem 4 rios contíguos em um mapa, onde @l@ é dado pela @pegaRios@
verificaRiosI :: [[Terreno]] -> Bool
verificaRiosI [] = True
verificaRiosI (l:ls) = if (tamanho l) == 4 then False else True

-- | Funçao que verifica se ja existem 5 estradas ou 5 relvas seguidas, onde @l@ é dado ou pela @pegaRel@ ou pela @pegaEst@
verificaERI :: [[Terreno]] -> Bool
verificaERI [] = True
verificaERI (l:ls) = if (tamanho l) == 5 then False else True

-- | Funçao que verifica se existem 5 troncos seguidos, onde @l@ é dado pela @pegaTronco@
verificaTroncoI :: [Obstaculo] -> Bool
verificaTroncoI [] = True
verificaTroncoI t = if (tamanho t) == 5 then False else True

-- | Funçao que verifica se existem 3 carros seguidos, onde @l@ é dado pela @pegaCarro@
verificaCarroI :: [Obstaculo] -> Bool
verificaCarroI [] = True
verificaCarroI t = if (tamanho t) == 3 then False else True

{- | Esta função assegura que dois rios contíguos tenham direções diferentes.
-}

velocidadeR :: Terreno -> Velocidade
velocidadeR (Rio v) = (-v)
velocidadeR t = 1

{- | Esta função atribui valores à nova __Estrada__ de modo a não ser igual à anterior.
-}

velocidadeE :: Terreno -> Velocidade
velocidadeE (Estrada v) = (-v)
velocidadeE t = 1
