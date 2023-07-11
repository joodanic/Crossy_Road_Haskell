{- |
Module      : Tarefa1_2022li1g022
Description : Validação de um mapa
Copyright   : João Carvalho <a94015@alunos.uminho.pt>
              Gonçalo Faria Gonçalves <a100833@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g022 where

import LI12223

{- | Esta é a função principal. É uma conjunção das funções : 'contiguosR' , 'converteMT', 'terrenosImp', 'verificaNenhumAll' , 'verificaComp' ,
'converteMLO' , 'verificaCarroAll' , 'verificaRios' , 'pegaRios' , 'verificaER' , 'pegaEst' , 'pegaRel', 
onde o mapa só é válido se todas estas funções forem válidas.
-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = True
mapaValido m@(Mapa l ot@((t,(o:os)):rs))  = (contiguosR (converteMT m)) && (terrenosImp ot) && (verificaNenhumAll ot) && (verificaComp l (converteMLO m)) && (verificaTroncoAll ot) && (verificaCarroAll ot) && (verificaRios (pegaRios (converteMT m))) && (verificaER (pegaEst (converteMT m))) && (verificaER (pegaRel (converteMT m)))

{- | Função que retorna uma lista dos terrenos do mapa. Esta função permite uma utilização mais fácil dos terrenos do mapa, como por exemplo nas
 funções 'contiguosR' e 'pegaRios'.
-}
converteMT :: Mapa -> [Terreno]
converteMT (Mapa l []) = []
converteMT (Mapa l ((t,(o:os)):rs)) = t:converteMT (Mapa l rs) 

{- | Função que retorna uma lista das listas de obstáculos do mapa. Esta função, assim como a de cima, também permite uma mais fácil utilização
das listas de obstáculos, como por exemplo na função 'verificaComp'.
-} 
converteMLO :: Mapa -> [[Obstaculo]]
converteMLO (Mapa l []) = []
converteMLO (Mapa l ((t,(os)):rs)) = os:converteMLO (Mapa l rs)

{- | Esta função é a responsável pela verificação de rios contíguos. Esta função recebe a lista de terrenos de um mapa(a partir da função
'converteMT') e verifica se o primeiro terreno é um rio e a cabeça da cauda da lista é um rio. Para além disso, também verifica se esses dois rios
têm direções diferentes com o auxílio da função 'verificaRC'.
-}
contiguosR :: [Terreno] -> Bool
contiguosR [] = True
contiguosR [_] = True
contiguosR (t:ts) = if (t==Rio (checkVelocidade t) ) && (head(ts) == Rio (checkVelocidade (head ts))) then (verificaRC t (head ts)) && contiguosR (ts) else contiguosR ts

-- | Esta função retira a velocidade de um __Rio__ ou de uma __Estrada__ para auxiliar na função que verifica se dois rios contíguos têm direções diferentes.
checkVelocidade :: Terreno -> Velocidade
checkVelocidade (Rio v) = v
checkVelocidade (Estrada v) = v
 
-- | Esta função verifica se dois __Rios__ contíguos têm direções diferentes, com o auxílio da função 'checkVelocidade'.
verificaRC :: Terreno -> Terreno -> Bool
verificaRC (Rio v) (Rio v1) = if (v>0 && v1<0) || (v<0 && v1>0) then True else False

{- | Esta função tem é composta por 3 funções, a 'verificaA', que verifica se as __Arvores__ estão na __Relva__, a 'verificaC' que verifica se 
os __Carros__ estão na Estrada e a 'verificaT' que verifica se os __Troncos__ estão no __Rio__.
-}
terrenosImp :: [(Terreno,[Obstaculo])] -> Bool
terrenosImp [] = True
terrenosImp ((t,l):ls) = (verificaA t l) && (verificaC t l) && (verificaT t l) && terrenosImp ls

-- | Esta função recebe um terreno e uma lista de obstáculos e verifica se as __Arvores__ se encontram na __Relva__ ou não.
verificaA :: Terreno -> [Obstaculo] -> Bool
verificaA _ [] = True
verificaA t (h:hs) |(t== (Rio (checkVelocidade t)) || (t==Estrada (checkVelocidade t))) &&  h == Arvore = False
                   |otherwise = verificaA t hs

-- | Esta função recebe um terreno e uma lista de obstáculos e verifica se os __Troncos__ se encontram no __Rio__ ou não.
verificaT :: Terreno -> [Obstaculo] -> Bool
verificaT _ [] = True
verificaT t (h:hs) |(t== (Relva) || (t==Estrada (checkVelocidade t))) && h == Tronco = False
                   |otherwise = verificaT t hs

-- | Esta função recebe um terreno e uma lista de obstáculos e verifica se os __Carros__ se encontram na __Estrada__ ou não.
verificaC :: Terreno -> [Obstaculo] -> Bool
verificaC _ [] = True
verificaC t (h:hs) |(t== (Rio (checkVelocidade t)) || (t== Relva)) && h == Carro = False
                   |otherwise = verificaC t hs

{- | Esta função recebe a lista de Terrenos e lista de Obstáculos e confere se existe um Obstaculo __Nenhum__ no mapa.
Esta função chama a 'verificaNenhum' que verifica se existe um Obstaculo __Nenhum__ na primeira lista de Obstáculos. Se não existir na 1ª lista,
esta função verifica se existe no resto das listas de Obstáculos, chamando novamente a 'verificaNenhum'. Se no final das listas de Obstáculos,
não houver, isso invalida o mapa ser válido.
-}
verificaNenhumAll :: [(Terreno,[Obstaculo])] -> Bool
verificaNenhumAll [] = True
verificaNenhumAll ((t,o):rs) = if (verificaNenhum o) == True then verificaNenhumAll rs else False

{- | Esta função verifica se existe pelo menos um Obstaculo __Nenhum__ numa dada lista de Obstáculos. Com a auxiliar 'somaN' que dá o número
de __Nenhum__ numa lista de Obstáculos, basta verificar se esse número é maior ou igual a 1.
-}
verificaNenhum :: [Obstaculo] -> Bool
verificaNenhum [] = True
verificaNenhum l = if somaN(l) >=1 then True else False

{- | Esta função auxilia a 'verificaNenhum' de tal modo que, isto dá o número de Obstáculos __Nenhum__ numa lista de Obstáculos.
Com isto, ela retorna o número de __Nenhum__ numa lista de Obstáculos.
-}
somaN :: [Obstaculo] -> Int
somaN [] = 0
somaN (h:hs) = if h == Nenhum then 1+somaN hs else somaN hs

{- | Esta função é responsável por verificar se a largura do mapa corresponde ao comprimento do mapa, onde este é dado pelo tamanho da lista de Obstáculos.
Com o auxílio da função 'soma', esta função calcula o comprimento de todas as listas de obstáculos do mapa, verificando que esse valor é igual à
largura do mesmo.
-}
verificaComp :: Int -> [[Obstaculo]] -> Bool
verificaComp _ [] = True
verificaComp l (o:os) = if l == soma(o) then verificaComp l os else False

-- | Funçao que faz a soma de todos os elementos de uma lista.
soma :: [a] -> Int
soma [] = 0
soma (h:hs) = 1+soma hs

{- | Esta função verifica se, numa lista de Obstáculos, existe um Tronco de tamanho 5, com auxílio das funções: 'verificaTronco' e 'pegaTronco'.
-}
verificaTroncoAll :: [(Terreno,[Obstaculo])] -> Bool
verificaTroncoAll [] = True
verificaTroncoAll ((t,o):rs) = if (verificaTronco (pegaTronco o)) == True then verificaTroncoAll rs else False

{- | Esta função auxilia a 'verificaTroncoAll' de tal modo que, esta compara se o resultante da 'tamanho' da 'pegaTronco' é >5.
Se for esse o caso, então existe um Tronco de comprimento 5 ou mais, o que invalida o mapa. Caso contrário, o mapa é válido.
-}
verificaTronco :: [[Obstaculo]] -> Bool
verificaTronco [] = True
verificaTronco (t:ts) = if (tamanho t) > 5 then False else verificaTronco ts

{- | Esta função recebe uma lista de Obstáculos e constrói uma lista de listas, onde cada lista é a lista formada pelo agrupamento de Troncos contíguos.
-} 
pegaTronco :: [Obstaculo] -> [[Obstaculo]]
pegaTronco [] = []
pegaTronco (t:ts) = if (t==Tronco) then (takeWhile (==t) (t:ts)):pegaTronco (dropWhile (==t) (t:ts)) else pegaTronco ts

{- | Esta função verifica se, numa lista de Obstáculos, existe um Carro de tamanho 3, com auxílio das funções: 'verificaCarro' e 'pegaCarro'.
-}
verificaCarroAll :: [(Terreno,[Obstaculo])] -> Bool
verificaCarroAll [] = True
verificaCarroAll ((t,o):rs) = if (verificaCarro (pegaCarro o)) == True then verificaCarroAll rs else False

{- | Esta função auxilia a 'verificaCarroAll' de tal modo que, esta compara se o resultante da 'tamanho' da 'pegaCarro' é >3.
Se for esse o caso, então existe um Carro de comprimento 3 ou mais, o que invalida o mapa. Caso contrário, o mapa é válido.
-}
verificaCarro :: [[Obstaculo]] -> Bool
verificaCarro [] = True
verificaCarro (t:ts) = if (tamanho t) > 3 then False else verificaCarro ts

{- | Esta função, semelhante à 'pegaTronco', recebe uma lista de Obstáculos e constrói uma lista de listas, onde cada lista é a lista formada
pelo agrupamento de Carros contíguos.
-}
pegaCarro :: [Obstaculo] -> [[Obstaculo]]
pegaCarro [] = []
pegaCarro (t:ts) = if (t==Carro) then (takeWhile (==t) (t:ts)):pegaCarro (dropWhile (==t) (t:ts)) else pegaCarro ts


{- | Esta função verifica se, dados uma lista de listas de terrenos, se existe uma lista tal que tenha 4 rios contíguos.
Isso é possível ao comparar o tamanho de cada lista resultante do agrupamento de rios pela função 'pegaRios' a 4.
Se esse valor for maior que 4, então o mapa torna-se inválido, caso contrário, o mapa é válido.
-}
verificaRios :: [[Terreno]] -> Bool
verificaRios [] = True
verificaRios (t:ts) = if (tamanho t) > 4 then False else verificaRios ts

{- | Esta função é a responsável por, dada uma lista de Terrenos, agrupar numa lista de listas os Rios contíguos. Esse resultado é possível
ao utilizar as funções @takeWhile@ e @dropWhile@, ou seja, ao percorrer a lista, a takeWhile vai criando uma lista de Rios enquanto eles forem
seguidos. Quando houver um __Terreno__ que não seja Rio, a dropWhile vai atravessando a lista até encontrar novamente um __Rio__, onde a 
takeWhile começa a criar uma lista de __Rio__, juntando à outra lista de __Rio__, já existente. 
-}
pegaRios :: [Terreno] -> [[Terreno]]
pegaRios [] = []
pegaRios (t:ts) = if (t==(Rio (velocidadef t))) then (takeWhile (==t) (velocidadeF (t:ts))):pegaRios (dropWhile (==t) (velocidadeF (t:ts))) else pegaRios ts

{-  |
-}
velocidadeF :: [Terreno] -> [Terreno]
velocidadeF [] = []
velocidadeF (t:ts) = if (t == Rio (checkVelocidade t)) then (Rio 1):velocidadeF ts else if (t == Estrada (checkVelocidade t)) then (Estrada 1):velocidadeF ts else t:velocidadeF ts

{-  |
-}

velocidadef :: Terreno -> Velocidade
velocidadef (Rio v) = 1
velocidadef (Estrada v) = 1

{-  | Esta função, com o auxílio da função 'tamanho', compara se existe uma lista de Estradas contíguas cujo tamanho seja maior que 5.
Se esse for o caso, o mapa torna-se inválido, caso contrário, o mapa é válido.
-}
verificaER :: [[Terreno]] -> Bool
verificaER [] = True
verificaER (t:ts) = if (tamanho t) > 5 then False else verificaER ts

-- | Esta função é responsável por agrupar numa lista de listas as Estradas contíguas.
pegaEst :: [Terreno] -> [[Terreno]]
pegaEst [] = []
pegaEst (t:ts) = if (t==(Estrada (velocidadef t))) then (takeWhile (==t) (velocidadeF (t:ts))):pegaEst (dropWhile (==t) (velocidadeF (t:ts))) else pegaEst ts

-- | Esta função é a função responsável por agrupar as Relvas contíguas numa lista de listas.
pegaRel :: [Terreno] -> [[Terreno]]
pegaRel [] = []
pegaRel (t:ts) = if (t==Relva) then (takeWhile (==t) (t:ts)):pegaRel (dropWhile (==t) (t:ts)) else pegaRel ts

-- | Esta função calcula o tamanho de uma lista.
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (h:hs) = 1+tamanho hs
