module Main where

import LI12223
import Tarefa1_2022li1g022
import Tarefa2_2022li1g022
import Tarefa3_2022li1g022
import Tarefa4_2022li1g022
import Tarefa5_2022li1g022    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit
-- | tipo onde são colocadas as coordenadas iniciais
type Estado = (Float, Float)
-- | tipo da pontuação
type Score = Int
-- | tipo 'world' com todos os parâmetros necessários para o jogo
type EstadoGloss = (Estado, Jogo, Texture,Int,Score)
-- | tipo que agrupa todas as texturas
type Texture = [Picture]
{- | função main que dá load às texturas e depois corre o jogo
-}
main :: IO ()
main = do
            carroD <-loadBMP "car1_sprL.bmp"
            chickenF <- loadBMP "chickenF.bmp"
            log <- loadBMP "log.bmp"
            road <- loadBMP "Road.bmp"
            tree <- loadBMP "Tree.bmp"
            water<- loadBMP "water.bmp"
            grass <- loadBMP "grass.bmp"
            playIO
               dm 
               white 
               fr
               (estadoGlossInicial 0.0 0.0 [carroD,log,tree,water,grass,road,chickenF] estadoInicial 0 25)
               desenhaMJ
               moveJogador1
               reageTempoGloss

-- | jogo inicial
j1 :: Jogo
j1 = (Jogo jog33 m2)
-- | mapa inicial
m2 :: Mapa
m2 = (Mapa 5 [(Relva, [Arvore, Arvore, Nenhum, Arvore,Arvore]),(Relva, [Arvore, Nenhum, Nenhum, Nenhum, Nenhum]),(Relva, [Arvore, Nenhum,Nenhum, Arvore, Arvore])])
-- | coordenadas iniciais do jogador
jog33 :: Jogador
jog33 = (Jogador (2,2))
-- | framerate 
fr :: Int
fr = 1
-- | função que centraliza o mapa
centraMapa :: Int -> Picture -> Picture
centraMapa f picture = Translate (-280) 80 picture

tamanhoPeca :: Float
tamanhoPeca = 180.0

estadoInicial :: Jogo
estadoInicial = j1

-- | função que recebe as coordenadas, as texturas, o jogo, o tempo e o score e agrupa tudo no estadoGloss
estadoGlossInicial :: Float -> Float -> Texture -> Jogo -> Int -> Score -> EstadoGloss
estadoGlossInicial x y tex j i score = ((x,y),j,tex,i,score)

dm :: Display
dm = InWindow "Novo Jogo" (1920,1080) (1920,1080)

-- | função que trasnforma um estadoGloss numa Picture
desenhaMJ :: EstadoGloss -> IO Picture
desenhaMJ ((x,y),(Jogo j m),tex,sec,score) = return $ (centraMapa (larguraMapa m) (Pictures [(desenhaMapa x y tex m),(desenhaJogador tex j),desenhaScore score]))
                                    
-- | função que adiciona ao o evento ao estadoGloss para o move poder ser desenhado
moveJogador1 :: Event -> EstadoGloss -> IO EstadoGloss
moveJogador1 e est@(f,j1@(Jogo j@(Jogador (x,y)) m@(Mapa l ((t,o):os))),tex,i,score) = return $ (f,(animaJogo j1 (moveJogador e)),tex,i,score)

-- | função que dá a largura de um mapa
larguraMapa :: Mapa -> Int
larguraMapa (Mapa l []) = 0
larguraMapa (Mapa l r) = l

-- | função que transforma um evento num move
moveJogador :: Event -> Jogada
moveJogador (EventKey (Char 'w') Down _ _)  = (Move Cima)
moveJogador (EventKey (Char 's') Down _ _) = (Move Baixo)
moveJogador (EventKey (Char 'd') Down _ _) = (Move Direita)
moveJogador (EventKey (Char 'a') Down _ _) = (Move Esquerda)
moveJogador (EventKey (SpecialKey KeyUp) Down _ _) = (Move Cima)
moveJogador (EventKey (SpecialKey KeyDown) Down _ _) = (Move Baixo)
moveJogador (EventKey (SpecialKey KeyRight) Down _ _) = (Move Direita)
moveJogador (EventKey (SpecialKey KeyLeft) Down _ _) = (Move Esquerda)
moveJogador _ = (Parado)

-- | função que desenha o jogador no mapa
desenhaJogador :: Texture -> Jogador -> Picture
desenhaJogador (carroD:log:tree:water:grass:road:chickenF:[]) (Jogador (x,y)) = Translate x' y' chickenF
            where x' = (fromIntegral x) * 180
                  y' = ((fromIntegral y) * 180)*(-1.0)

-- | função que desenha o mapa por inteiro, com auxílio da @desenhaTerrenos@ e da @desenhaObstaculos@
desenhaMapa :: Float -> Float -> Texture -> Mapa -> Picture
desenhaMapa _ _ _ (Mapa l []) = Blank
desenhaMapa x y tex m@(Mapa l r) = Pictures [(desenhaTerrenos x y tex m),(desenhaObstaculos x y tex m)]

-- | função responsável por desenhar todos os terrenos de um mapa
desenhaTerrenos :: Float -> Float -> Texture -> Mapa -> Picture
desenhaTerrenos _ _ _ (Mapa l []) = Blank
desenhaTerrenos x y tex (Mapa l ((t,o):r)) = Pictures [(desenhaTerreno l x y tex t),(desenhaTerrenos newx newy tex (Mapa l r))]
            where newx = 0.0
                  newy = y-tamanhoPeca

-- | função que desenha 1 linha de terreno, com a ajuda da @desenhaTer@
desenhaTerreno :: Int-> Float -> Float -> Texture -> Terreno -> Picture
desenhaTerreno 0 _ _ _ _ = Blank
desenhaTerreno l x y tex terreno = Pictures [desenhaTer x y tex terreno, desenhaTerreno (l-1) newx y tex terreno]
                                    where newx = x+tamanhoPeca
-- | função que desenha 1 terreno
desenhaTer :: Float -> Float -> Texture -> Terreno -> Picture
desenhaTer x y (carroD:log:tree:water:grass:road:chickenF:[]) h =
          case h of
                Rio n -> Translate x y $ Scale 1 1 $ water
                Estrada n -> Translate x y $ Scale 1 1 $ road
                Relva -> Translate x y $ Scale 1 1 $ grass

-- | função que desenha uma linha completa do mapa, com a ajuda da @desenhaLinha@ e @desenhaObstaculos.
desenhaObstaculos :: Float -> Float -> Texture -> Mapa -> Picture
desenhaObstaculos x y e (Mapa _ []) = Blank
desenhaObstaculos x y e@(carroD:log:tree:water:grass:road:chickenF:[]) (Mapa l ((t,o):r)) = Pictures [(desenhaLinha x y e o),(desenhaObstaculos newx newy e (Mapa l r))]
             where newx = 0.0
                   newy = y-tamanhoPeca

-- | função que desenha uma Lista de Obstáculo com a ajuda da @desenhaPeca@
desenhaLinha :: Float -> Float -> Texture -> [Obstaculo] -> Picture
desenhaLinha x y t [] = Blank
desenhaLinha x y e@(carroD:log:tree:water:grass:road:chickenF:[]) (o:r) = Pictures [(desenhaPeca x y e o), desenhaLinha x1 y1 e r]
              where x1 = x+tamanhoPeca
                    y1 = y

-- | função repsonsável por desenhar cada tipo de Obstáculo
desenhaPeca :: Float -> Float -> Texture -> Obstaculo -> Picture
desenhaPeca x y e@(carroD:log:tree:water:grass:road:chickenF:[]) o =
          case o of
              Carro -> Translate x' y' $ Scale 1 1 $ carroD
              Arvore -> Translate x' y' $ Scale 1 1 $ tree
              Tronco -> Translate x' y' $ Scale 1 1 $ log
              _ -> Blank
          where x' = (x)
                y' = (y)

-- | função responsável por ir atualizando o mapa a cada iteração, tanto para ver se o jogador perdeu ou se atualiza o mapa
reageTempoGloss :: Float -> EstadoGloss -> IO EstadoGloss
reageTempoGloss n s@((x,y),jog1@(Jogo j m),tex,sec,score) = if (jogoTerminou jog1) then 
                                                                       do (die ("PERDESTE com " ++ show score ++ " pontos"))
                                                                       else if ((mapaValido m) == False) then 
                                                                       do (die "MAPA INVALIDO")
                                                                       else return $ ((x,y),(Jogo j1 m1),tex,(sec+1),(obtemScore score)) 
                                    where (Jogo j1 m1) = verificaSecs s

-- | função que gera um número aleatório a ser passad à @verificaSecs@ onde esse número ditará qual será a nova linha a ser adicionada
generateRandoms :: Int -- ^ numero de inteiros que pretendemos gerar
                   -> Int -- ^ seed
                   -> [Int]
generateRandoms n seed = let gen = mkStdGen seed -- creates a random generator
                        in take n $ randomRs (1,100) gen

-- | função que retorna o número dentro da lista
listToInt :: [Int] -> Int
listToInt [] = 0
listToInt [x] = x

-- | função responsável por só atualizar o mapa de duas em duas iterações, o que permite o jogador poder avançar na eventualidade de precisar se desviar de um obstáculo.
verificaSecs :: EstadoGloss -> Jogo
verificaSecs ((x,y),j@(Jogo (Jogador (x1,y1)) m),tex,sec,_) = if (even sec) then (Jogo (Jogador (x1,y1)) (shift1 m)) else if (odd sec) && (mapaValido m1) then (Jogo j1 (shift1 m1)) else deslizaJogo (n-1) j
            where (Jogo j1 m1) = deslizaJogo n j
                  n = (listToInt (generateRandoms 1 seed))
                  seed = (x1+y1)*(x1+y1)


-- | função responsável por desenhar o score à medida que o jogador vai avançando no jogo.
desenhaScore :: Int -> Picture
desenhaScore y = Translate (-600) 200 $ Scale 1 1 $ (Text (show (obtemScore y)))

-- | função que vai atualizando o score.
obtemScore :: Int -> Int
obtemScore y = y+25
