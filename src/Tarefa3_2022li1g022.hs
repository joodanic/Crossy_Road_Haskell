{- |
Module      : Tarefa3_2022li1g022
Description : Movimentação do personagem e obstáculos
Copyright   : João Carvalho <a94015@alunos.uminho.pt>
              Gonçalo Faria Gonçalves <a100833@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g022 where

import LI12223

-- | Esta função, é a função responsável por atualizar o jogo sempre que há uma jogada
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo j1@(Jogador (x,y)) m@(Mapa l os@((t,o):r))) jogad
                     |(jogad == Parado) && (emCimaTL (converteobs os (0,0)) j1) = (Jogo (moveT j1 (emCimatl (converteobs os (0,0)) j1)) (shift1(m)))
                     |(moveValidoObs jogad (converteL (encontraSL (converteobs os (0,0)) j1 jogad))) && (moveValido j1 jogad (length os) (length o)) && ((emCimaTL (converteobs os (0,0)) (move (j1) jogad)) == False) = (Jogo (move(j1) jogad) (shift1(m)))
                     |(moveValidoObs jogad (converteL (encontraSL (converteobs os (0,0)) j1 jogad))) && (moveValido j1 jogad (length os) (length o)) && (emCimaTL (converteobs os (0,0)) (move (j1) jogad)) = (Jogo (moveT (move (j1) jogad) (emCimatl (converteobs os (0,0)) j1)) (shift1(m)))
                     |otherwise = (Jogo j1 (shift1(m)))
-- | Função que trabalha com o __Mapa__.
shift1 :: Mapa -> Mapa
shift1 (Mapa l os@((t,o):rs)) = (Mapa l (shiftAll os))

-- | Função auxiliar que atualiza o mapa todo.
shiftAll :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
shiftAll [] = []
shiftAll (l:ls) = (shiftOne l):shiftAll ls

{- | Função que atualiza o mapa linha a linha, de tal maneira que, se o __Terreno__ for __Relva__, ele não mexe, caso contrário, 
pega no valor da velocidade e se ela for menor que 0, chama a 'shiftE', caso contrário, chama a 'shiftD'.
-}
shiftOne :: (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
shiftOne (t,[]) = (t,[])
shiftOne r@(Rio v,o) = if v>0 then (shiftD r v) else (shiftE r (-v))
shiftOne e@(Estrada v,o) = if v>0 then (shiftD e v) else (shiftE e (-v))
shiftOne (Relva,o) = (Relva,o)

{- | Função que atualiza a linha da direita para a esquerda, com o auxílio da @splitAt@ que separa a lista de __Obstáculos__ em duas e depois basta
concatenar a 2ª lista com a 1ª.
-}
shiftE :: (Terreno,[Obstaculo]) -> Int -> (Terreno,[Obstaculo])
shiftE (t,[]) x = (t,[])
shiftE (t,l) x = if x <= (length l) then (t,lb ++ la) else shiftE (t,l) newx
      where (la, lb) = splitAt x l
            newx= x-(length l)


-- | Função que atualiza a linha da esquerda para a direita, semelhante à 'shiftE', mas com o efeito contrário.
shiftD :: (Terreno,[Obstaculo]) -> Int -> (Terreno,[Obstaculo])
shiftD (t,[]) x = (t,[])
shiftD (t,l) x = if x <= (length l) then (t,lb ++ la) else shiftD (t,l) newx
      where (la, lb) = splitAt (length l - x) l
            newx = x-(length l)

{- | Função responsável por mover o __Jogador__ em cima de um __Tronco__
-}

moveT :: Jogador -> Velocidade -> Jogador
moveT (Jogador (x,y)) v = (Jogador (x+v,y))

{- | Função responsável por mover o __Jogador__ pelo __Mapa__.
-}
move :: Jogador -> Jogada -> Jogador
move j Parado = j
move (Jogador (x,y)) (Move Cima) = (Jogador (x,y-1))
move (Jogador (x,y)) (Move Baixo) = (Jogador (x,y+1))
move (Jogador (x,y)) (Move Direita) = (Jogador (x+1,y))
move (Jogador (x,y)) (Move Esquerda) = (Jogador (x-1,y))

{- | Função responsável verificar se o __Jogador__ não se tenta mover para o mesmo espaço que uma __Arvore__ ou um __Carro__.
-}

moveValidoObs :: Jogada -> Obstaculo -> Bool
moveValidoObs j o = if ((o == Arvore) || (o == Carro)) then False else True

{- | Função que verifica se o __Jogador__ não se tenta mover para fora do __Mapa__.
-}
moveValido :: Jogador -> Jogada -> Int -> Int -> Bool
moveValido _ Parado _ _ = True
moveValido (Jogador (x,y)) (Move Cima) a l = if y==0 then False else True
moveValido (Jogador (x,y)) (Move Baixo) a l = if (a-1)<=y then False else True
moveValido (Jogador (x,y)) (Move Esquerda) a l = if x == 0 then False else True
moveValido (Jogador (x,y)) (Move direita) a l = if (l-1)<=x then False else True

{- | Função que verifica se existe um __Obstáculo__ após o move do __Jogador__, com auxílio da 'encontraS'
-}
encontraSL :: [[(Terreno,Obstaculo,Coordenadas)]] -> Jogador -> Jogada -> [Obstaculo]
encontraSL [] _ _ = []
encontraSL (l:ls) j@(Jogador (x,y)) jogad = if encontraS l j jogad == [] then encontraSL ls j jogad else encontraS l j jogad

{- | Função que retorna a lista de __Obstáculos__ na posição para a qual o __Jogador__ se movimentaria, por exemplo, se o __Jogador__
se movimentasse para a __Direita__ e nessa posição estaria um __Obstáculo__, a função daria esse __Obstáculo__, caso contrário, tentaria para a
próxima posição.
-}

encontraS :: [(Terreno,Obstaculo,Coordenadas)] -> Jogador -> Jogada -> [Obstaculo]
encontraS [] _ _ = []
encontraS ((t,o,(x1,y1)):hs) (Jogador (x,y)) (Move Direita) = if (x1,y1)==(x+1,y) then [o] else encontraS hs (Jogador (x,y)) (Move Direita)
encontraS ((t,o,(x1,y1)):hs) (Jogador (x,y)) (Move Esquerda) = if (x1,y1)==(x-1,y) then [o] else encontraS hs (Jogador (x,y)) (Move Esquerda)
encontraS ((t,o,(x1,y1)):hs) (Jogador (x,y)) (Move Cima) = if (x1,y1+1) == (x,y) then [o] else encontraS hs (Jogador (x,y)) (Move Cima)
encontraS ((t,o,(x1,y1)):hs) (Jogador (x,y)) (Move Baixo) = if (x1,y1-1) == (x,y) then [o] else encontraS hs (Jogador (x,y)) (Move Baixo)
encontraS _ _ _ = []

velJ :: Velocidade -> Jogada
velJ v = if v<0 then (Move Esquerda) else (Move Direita)

{- | Função que verifica se o __Jogador__ se encontra em cima de um __Tronco__, ao longo do __Mapa__, com o auxílio da função 'emCimat'.
-}

emCimatl :: [[(Terreno,Obstaculo,Coordenadas)]] -> Jogador -> Velocidade
emCimatl [] _ = 0
emCimatl (l:ls) j = if ((emCimat l j)==0) then emCimatl ls j else emCimat l j

{- | Função que verifica se o __Jogador__ se encontra em cima de um __Tronco__ ao longo da coluna, ao verificar se o __Terreno__ é um __Rio__,
se o __Obstáculo__ é um __Tronco__ e se as coordenadas do __Tronco__ são as mesmas que as do __Jogador__, devolvendo a __Velocidade__ de modo
a se poder atualizar caso o __Jogador__ fique __Parado__ em cima do __Tronco__.
-}

emCimat :: [(Terreno,Obstaculo,Coordenadas)] -> Jogador -> Velocidade
emCimat [] _ = 0
emCimat ((t,o,(x1,y1)):hs) (Jogador (x,y)) = if (t == (Rio (checkVelocidade' t))) && (o == Tronco) && (x1,y1) == (x,y) then (checkVelocidade' t) else emCimat hs (Jogador (x,y))

{- | Função responsável por verificar se o __Jogador__ se encontra em cima de um __Tronco__ ao longo do __Mapa__, com o auxílio da 'emCimaT'.
-}

emCimaTL :: [[(Terreno,Obstaculo,Coordenadas)]] -> Jogador -> Bool
emCimaTL [] _ = False
emCimaTL (l:ls) j = if (emCimaT l j) then True else emCimaTL ls j 

{- | Função que verifica se o __Jogador__ se encontra em cima de um __Tronco__ ao longo das colunas. Com isto, será possível usar a 'emCimatl'.
-}

emCimaT :: [(Terreno,Obstaculo,Coordenadas)] -> Jogador -> Bool
emCimaT [] _ = False
emCimaT ((t,o,(x1,y1)):hs) (Jogador (x,y)) = if (t == (Rio (checkVelocidade' t))) && (o == Tronco) && (x1,y1) == (x,y) then True else emCimaT hs (Jogador (x,y))

-- | Funçao que atribui coordenadas a cada uma das linhas do __Mapa__, com o auxílio da função 'converteO'.
converteobs :: [(Terreno,[Obstaculo])] -> Coordenadas -> [[(Terreno,Obstaculo,Coordenadas)]]
converteobs [] (x,y) = []
converteobs (h:t) (x,y) = converteO h (x,y):converteobs t newC
          where newC = (x,y+1)

-- | Funçao que atribui coordenadas a uma linha do __Mapa__.
converteO :: (Terreno,[Obstaculo]) -> Coordenadas -> [(Terreno,Obstaculo,Coordenadas)]
converteO (t,[]) (x,y) = []
converteO (t,(h:rs)) (x,y) = (t,h,(x,y)):converteO (t,rs) newC
        where newC = (x+1,y)

{- | Função que, dada uma lista de __Obstaculo__, ou dá a cabeça da lista ou devolve __Nenhum__ caso a lista esteja vazia, utilizada pela
'moveValidoObs'.
-}

converteL :: [Obstaculo] -> Obstaculo
converteL [] = Nenhum
converteL (h:t) = h

-- | Retira a __Velocidade__ de um __Rio__ e de uma __Estrada__.
checkVelocidade' :: Terreno -> Velocidade
checkVelocidade' (Rio v) = v
checkVelocidade' (Estrada v) = v

