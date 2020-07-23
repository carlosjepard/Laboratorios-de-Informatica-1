-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g075 where

import LI11819
import Tarefa0_2018li1g075
-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [(Estado ms js1 dy),(Estado ms js1 dr),(Estado ms js1 de),(Estado ms js1 da),(Estado ms js1 di)]
-- | Alguns Testes

ms = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia
   ,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel
   ,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia
   ,Vazia,Bloco Indestrutivel]
  ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel
   ,Bloco Indestrutivel]]
-- | testes
dy = [disparo1,disparo3,disparo4]
disparo1 = DisparoCanhao 0 (3,1) B
disparo2 = DisparoLaser  1 (5,2) D
disparo3 = DisparoCanhao 2 (6,4) E
disparo4 = DisparoChoque 3 2
disparo5 = DisparoLaser  0 (2,1) B
disparo6 = DisparoChoque 1 3
js1 = [jogador11,jogador22,jogador33,jogador44]
jogador11 = Jogador (1,1) B 2 1 0
jogador22 = Jogador (5,1) E 1 0 2
jogador33 = Jogador (6,5) E 0 2 2
jogador44 = Jogador (1,5) C 3 0 0
-- | testes
dr = [disparo1,disparo3,disparo4,disparo5,disparo6]
de = [disparo5,disparo2]
di = [disparo3]
da = [disparo4,disparo6]
-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers l = tickLaserss l (contador l)
-- | contador para o numero de elementos da lista disparos funcao principal que modifica o estado dado um DLaser
tickLaserss :: Estado -> Int -> Estado
tickLaserss l 0 = l
tickLaserss (Estado e j ((DisparoCanhao a b c):xs)) cont =tickLaserss (Estado e j (meteC (DisparoCanhao a b c) xs )) (cont-1)
tickLaserss (Estado e j ((DisparoChoque a z):xs)) cont =tickLaserss (Estado e j (meteC (DisparoChoque a z) xs)) (cont-1)
tickLaserss (Estado e j ((DisparoLaser a (x,y) d):xs))cont =tickLaserss (Estado (converte(destroi(corrigirPM(pega (x,y) d e) d e))e)   (jogador j (x,y) d e)    (destroiBala xs (unconcat(pega (x,y) d e)))) (cont-1)





-- | coloca um disparo no fim de uma lista de Disparos
meteC :: Disparo -> [Disparo] -> [Disparo]
meteC a [] = [a]
meteC a (x:xs) = x:meteC a xs
-- | muda o estado do jogo no mapa
converte :: [(Posicao,Peca)] -> Mapa  -> Mapa
converte [] x = x
converte (((x,y),a):xs) e = (converte xs (atualizaPosicaoMatriz (x,y) a e))

-- | funcao que corrige os lasers para  mais do que um linha
corrigirPM :: [(Posicao,Peca)] ->Direcao -> Mapa -> [(Posicao,Peca)]
corrigirPM [] d e = []
corrigirPM (((a,b),y):xs) d e | d == C =  ((a,b),y):((a,b+1),(encontraPosicaoMatriz (a,b+1) e)):corrigirPM xs d e
                              | d == B = ((a,b),y):((a,b+1),(encontraPosicaoMatriz (a,b+1) e)):corrigirPM xs d e
                              | d == E = ((a,b),y):((a+1,b),(encontraPosicaoMatriz (a+1,b) e)):corrigirPM xs d e
                              | d == D = ((a,b),y):((a+1,b),(encontraPosicaoMatriz (a+1,b) e)):corrigirPM xs d e


-- | modifica as pecas do mapa dado um DLaser
destroi :: [(Posicao,Peca)] -> [(Posicao,Peca)]
destroi [] = []
destroi ((x,y):xs) | y == Bloco Destrutivel = (x,Vazia) : destroi xs
                   | y == Vazia = (x,y) : destroi xs
                   | otherwise = xs
                  
-- | gera a lista de (Posicoes, Pecas) referentes a um Disparo de um jogador, 
pega :: Posicao -> Direcao -> Mapa -> [(Posicao,Peca)]
pega (x,y) d e |(d == C) = takeMLC (x,y) e
               |(d == E) = takeMCE (x,y) e
               |(d == D) = takeMCD (x,y) e 
               |(d == B) = takeMCB (x,y) e 
               |otherwise = undefined 
-- | dada uma posicao gera uma lista de Posica,peca dada a direcao C
takeMLC :: Posicao -> Mapa -> [(Posicao,Peca)]
takeMLC (x,y) l | (x==0) = []
                |otherwise = ((x,y),encontraPosicaoMatriz (x,y) l): takeMLC (x-1,y) l
-- | dada uma posicao gera uma lista de Posica,peca dada a direcao E
takeMCE :: Posicao -> Mapa ->[(Posicao,Peca)]
takeMCE (x,y) l | (y==0) = []
                |otherwise = ((x,y),encontraPosicaoMatriz (x,y) l) : takeMCE (x,y-1) l
-- | dada uma posicao gera uma lista de Posica,peca dada a direcao D
takeMCD :: Posicao -> Mapa -> [(Posicao,Peca)]
takeMCD (x,y) l | (y==((length (head l))-1)) = []
                |otherwise = ((x,y),encontraPosicaoMatriz (x,y) l): takeMCD (x,y+1) l               
-- | da o tamanho de uma linha 
tamanho1 :: Mapa -> Int -> Int
tamanho1 ([]:s) x = x 
tamanho1 ((x:xs):y) a = tamanho (xs:y) a+1

-- | dada uma posicao gera uma lista de Posica,peca dada a direcao B
takeMCB :: Posicao -> Mapa -> [(Posicao,Peca)]
takeMCB (x,y) l | (x==((tamanho l 0)-1)) = [] 
                |otherwise = ((x,y),encontraPosicaoMatriz (x,y) l) : takeMCB (x+1,y) l 

-- | da o tamanho de uma coluna
tamanho :: Mapa -> Int -> Int 
tamanho (ls:[]) x = x
tamanho ((a:b):c) x  = tamanho c  x+1
-- | list de posicoes para mudar o estado de jogo em relacao aos jogadores e disparos
unconcat :: [(Posicao,Peca)] -> [Posicao]
unconcat [] = []
unconcat ((x,y):xs) = x : unconcat  xs
-- | muda o jogador funcao principal
jogador :: [Jogador] ->Posicao -> Direcao -> Mapa -> [Jogador]
jogador [] _ _ _  = []
jogador (h:t) (x,y) d e = (vidaJog (unconcat(pega (x,y) d e)) h) :(jogador t (x,y) d e)

-- | dada uma lista de posicoes, decrementa a vida caso alguma dessas posicoes corresponde a algum jogador
vidaJog :: [Posicao] -> Jogador -> Jogador
vidaJog [] j = j
vidaJog ((x,y):xs) (Jogador (a,b) d v l c) | (x,y) == (a,b) = (Jogador (a,b) d (v-1) l c)
                                           | otherwise = vidaJog xs (Jogador (a,b) d v l c)

-- | muda os disparos, funcsao principal 

destroiBala :: [Disparo] -> [Posicao] -> [Disparo]
destroiBala [] (x:xs) = []
destroiBala ((DisparoLaser x (a,b) d):xs) l =(DisparoLaser x (a,b) d): destroiBala xs l
destroiBala ((DisparoChoque x d):xs) l =(DisparoChoque x d): destroiBala xs l
destroiBala ((DisparoCanhao x (a,b) d):xs) (y:ys) | (compararPosD (DisparoCanhao x (a,b) d) (y:ys)) = (destroiBala xs (y:ys))
                                                  | otherwise =    (DisparoCanhao x (a,b) d) : (destroiBala xs (y:ys))                           
-- | compara um DCanhao com a lista de posicoes afetadas pelo laser, dy True caso a posicso da bala corresponde a area afetada pelo lsaser
compararPosD :: Disparo -> [Posicao] -> Bool
compararPosD j [] = False
compararPosD (DisparoCanhao x (a,b) d) (y:ys) | ((a,b) == y) = True
                                              | otherwise = compararPosD  (DisparoCanhao x (a,b) d) ys



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado 
tickCanhoes l = tickCanhoess l (contador l)

-- | modifica o estado dado um disparo canhao utilizando um contador  de disparos, ignoras os outros disparos,mantem na lista e decrementa
tickCanhoess :: Estado -> Int ->  Estado
tickCanhoess l 0 = l
tickCanhoess (Estado e j ((DisparoLaser a b c):xs)) cont = tickCanhoess (Estado e j (meteC (DisparoLaser a b c) xs )) (cont-1)
tickCanhoess (Estado e j ((DisparoChoque a z):xs)) cont = tickCanhoess (Estado e j (meteC (DisparoChoque a z) xs)) (cont-1)
tickCanhoess (Estado e j ((DisparoCanhao a (x,y) d):xs)) cont  | (verificaM (listap(auxmapa (x,y) d e))) &&  (verificaJ (pegaJ (x,y) d) j) = (tickCanhoess (Estado e j (meteC (movimentaBala(DisparoCanhao a (x,y) d)) xs )) (cont-1) )
                                                               | otherwise = tickCanhoess (Estado (converte(destroi(listap (auxmapa (x,y) d e)))e)   (jogadorBC (pegaJ (x,y) d) j)   (modificaDC (x,y) d ((DisparoCanhao a (x,y) d):xs))) (cont-1)
-- | gera o tamanho da lista de disparos
contador :: Estado -> Int
contador (Estado e j ls) = length ls
-- | muda mapa
verificaM :: [(Posicao,Peca)] -> Bool
verificaM [] = True
verificaM ((x,y):xs) = if y== Vazia then verificaM xs else False


-- | usa o par de posicoes que podem ser afetados pela balacanhao e a lista de jogadores, da falso caso a bala tenha de explodir
verificaJ :: (Posicao,Posicao) -> [Jogador] -> Bool
verificaJ (x,y) [] = True
verificaJ (x,y) ((Jogador a b c d e):xs) = if (x == a) || (y == a) then False else verificaJ (x,y) xs


-- | dada a posicao de uma bala e a direcao, da os 2 par de (posicao,peca) que podem ser afetados no mapa 
auxmapa :: Posicao -> Direcao -> Mapa -> ((Posicao,Peca),(Posicao,Peca))
auxmapa (x,y) C e = (((x-1,y),encontraPosicaoMatriz (x-1,y) e),((x-1,y+1),encontraPosicaoMatriz (x-1,y+1) e))
auxmapa (x,y) D e = (((x,y+1),encontraPosicaoMatriz (x,y+1) e),((x+1,y+1),encontraPosicaoMatriz (x+1,y+1) e))
auxmapa (x,y) E e = (((x,y),encontraPosicaoMatriz (x,y) e),((x+1,y),encontraPosicaoMatriz (x+1,y) e))
auxmapa (x,y) B e = (((x+1,y),encontraPosicaoMatriz (x+1,y) e),((x+1,y+1),encontraPosicaoMatriz (x+1,y+1) e))


-- | transforma o par de cima numa lista (auxmapa)
listap :: ((Posicao,Peca),(Posicao,Peca)) -> [(Posicao,Peca)]
listap ((x,y),(a,b)) = [(x,y),(a,b)]




                           
-- | movimenta a bala de canhao caso nao haja nada que a faca explodir
movimentaBala :: Disparo -> Disparo
movimentaBala (DisparoCanhao a (x,y) C) = (DisparoCanhao a (x-1,y) C)
movimentaBala (DisparoCanhao a (x,y) B) = (DisparoCanhao a (x+1,y) B)
movimentaBala (DisparoCanhao a (x,y) E) = (DisparoCanhao a (x,y-1) E)
movimentaBala (DisparoCanhao a (x,y) D) = (DisparoCanhao a (x,y+1) D)
                                       
-- | dada a posicao atual da bala e a direcao, gera a par de posicoes em que um jogador pode ser afetado por ela


pegaJ :: Posicao -> Direcao -> (Posicao,Posicao)
pegaJ (x,y) d   |(d == C) = ((x-1,y-1),(x-1,y+1))
                |(d == E) = ((x-1,y-1),(x+1,y-1))
                |(d == D) = ((x-1,y+1),(x+1,y+1))
                |(d == B) = ((x-1,y-1),(x+1,y+1))
              



-- |  dado um par de posicoes e uma lista de jogadores, decrementa caso ou uma ou oura posicao seja igual
jogadorBC :: (Posicao,Posicao)-> [Jogador] -> [Jogador]
jogadorBC (x,y) [] = []
jogadorBC (x,y) ((Jogador a b c d e):xs) | (x == a) || (y == a) = (Jogador a b (c-1) d e):jogadorBC (x,y) xs
                                         | otherwise = (Jogador a b c d e): jogadorBC (x,y) xs



-- | dada a posicao da bala e a direcao da o par de posicoes em que outras balas podem ser afetadas
posicaoD :: Posicao -> Direcao -> (Posicao,Posicao)
posicaoD (x,y) d   |(d == C) = ((x,y),(x+1,y))
                   |(d == E) = ((x,y),(x,y+1))
                   |(d == D) = ((x,y),(x,y-1))
                   |(d == B) = ((x,y),(x+1,y))
                   |otherwise = undefined
 

-- | modifica a lista de disparos caso alguma posicao de algum disparo coincida com a posicao dada
modificaDC :: Posicao -> Direcao -> [Disparo] -> [Disparo]
modificaDC p d [] = []
modificaDC p d ((DisparoChoque a b):xs) = (DisparoChoque a b) : modificaDC p d xs
modificaDC p d ((DisparoLaser a b c):xs)= (DisparoLaser a b c): modificaDC p d xs
modificaDC p d ((DisparoCanhao a (x,y) e):xs) | comparaPosDC (posicaoD p d) (x,y) = modificaDC p d xs
                                              | otherwise = (DisparoCanhao a (x,y) e) : modificaDC p d xs
-- | compara o par de posicoes que podem ser afetados, da true caso algum deles seja igual
comparaPosDC :: (Posicao,Posicao) -> Posicao -> Bool
comparaPosDC (x,y) (a,b) | (x == (a,b)) || (y == (a,b)) = True
                         | otherwise = False                                                                                   



-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques l = tickChoquess l (contador l)
-- | funcao princial de tickchoques que tem um contador para a lista de disparos e modifica o choque ou seja decrementa um tick 
tickChoquess :: Estado ->Int -> Estado
tickChoquess l 0 = l
tickChoquess (Estado e j ((DisparoCanhao a b c):xs))cont= tickChoquess (Estado e j (meteC (DisparoCanhao a b c) xs )) (cont-1)
tickChoquess (Estado e j ((DisparoLaser a b c):xs)) cont= tickChoquess (Estado e j (meteC (DisparoLaser a b c) xs)) (cont-1)
tickChoquess (Estado e j ((DisparoChoque a z ):xs)) cont | z == 0 = tickChoquess (Estado e j xs) (cont-1)
                                                         | otherwise = tickChoquess (Estado e j (meteC (DisparoChoque a (z-1)) xs)) (cont-1)





