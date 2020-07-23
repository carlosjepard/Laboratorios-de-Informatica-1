-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g075 where

import LI11819
import Tarefa0_2018li1g075

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(1,(Movimenta D),(Estado m js ds)), (0,(Dispara Canhao), (Estado m js ds))]

-- * Funções principais da Tarefa 2.

comeca_Disparo :: Jogada -> Int -> Jogador -> Estado -> [Disparo]
comeca_Disparo (Movimenta x) a b (Estado m (h:t) []) = [] 
comeca_Disparo (Dispara Canhao) x (Jogador a b c d e) m = [(DisparoCanhao x (somaVetores a (direcaoParaVetor b)) b )]
comeca_Disparo (Dispara Laser) x (Jogador a b c d e) m = [(DisparoLaser x (somaVetores a (direcaoParaVetor b)) b )]
comeca_Disparo (Dispara Choque) x (Jogador a b c d e) m = [(DisparoChoque x 5)] 

-- | Efetua uma jogada.
jogada :: Int -> Jogada -> Estado -> Estado
jogada x a (Estado m (h:t) []) | (eIndiceListaValido x (h:t)) = (Estado m (atualizaIndiceLista x (atualizaJogador a (numero x (h:t)) m) (h:t)) (comeca_Disparo a x (numero x (h:t)) (Estado m (h:t) [])))
                               | otherwise = (Estado m (h:t) [])
jogada x a (Estado m (h:t) (y:yt)) | (eIndiceListaValido x (h:t)) && (eIndiceListaValido x (y:yt)) = (Estado m (atualizaIndiceLista x (atualizaJogador a (numero x (h:t)) m) (h:t)) (colocaList (y:yt) (atualizaDisparo a x (numero x (h:t)) (Estado m (h:t) (y:yt))) a))
                                   | otherwise = (Estado m (h:t) (y:yt))


-- | Recebe uma jogada, jogador, mapa e, consoante estes, atualiza o jogador
atualizaJogador :: Jogada -> Jogador -> Mapa -> Jogador
atualizaJogador (Movimenta x) (Jogador a b c d e) m  | (b == x) && (c > 0) && ((verificaMapa (Movimenta x) a m) == True) = (Jogador (somaVetores (direcaoParaVetor x) a) b c d e)
                                                     | (b == x) && (c > 0) && ((verificaMapa (Movimenta x) a m) == False) = (Jogador a b c d e)
                                                     | (b /= x) && (c > 0) = (Jogador a x c d e)
                                                     | otherwise = (Jogador a b c d e)
atualizaJogador (Dispara Canhao) (Jogador a b c d e) m | (c>0) = (Jogador a b c d e)
                                                       | otherwise = (Jogador a b c d e)
atualizaJogador (Dispara Laser) (Jogador a b c d e) m | (c>0) && (d>0) = (Jogador a b c (d-1) e)
                                                      | otherwise = (Jogador a b c d e)
atualizaJogador (Dispara Choque) (Jogador a b c d e) m | (c>0) && (d>0) = (Jogador a b c d (e-1))
                                                       | otherwise = (Jogador a b c d e) 

-- | Numa lista de jogadores, pega no Jogador numero n
numero :: Int -> [Jogador] -> Jogador
numero n (x:xs) | n==0 = x
                | otherwise = numero (n-1) xs

-- | Pega no Disparo n da lista de Disparos
escolheDisp :: Int -> [Disparo] -> Disparo
escolheDisp n (x:xs) | n==0 = x
                     | otherwise = escolheDisp (n-1) xs

--adiciona_PRdisparo

                   
-- | Atualiza o Disparo consoante a jogada feita
atualizaDisparo :: Jogada -> Int -> Jogador -> Estado -> Disparo
atualizaDisparo (Movimenta x) a b (Estado m (h:t) (y:yt)) = escolheDisp a (y:yt) 
atualizaDisparo (Dispara Canhao) x (Jogador a b c d e) m = (DisparoCanhao x (somaVetores a (direcaoParaVetor b)) b )
atualizaDisparo (Dispara Laser) x (Jogador a b c d e) m = (DisparoLaser x (somaVetores a (direcaoParaVetor b)) b )
atualizaDisparo (Dispara Choque) x (Jogador a b c d e) m = (DisparoChoque x 5)

-- | Atualiza a Lista de Disparos
colocaList :: [Disparo] -> Disparo -> Jogada -> [Disparo]
colocaList (x:xs) a b | ((b == (Movimenta D) || b == (Movimenta C) || b == (Movimenta B) || b == (Movimenta E)) && (length (x:xs)) >= 0) = (x:xs)
                      | otherwise = a:x:xs
{-
colocaList [] a b v | (b == (Movimenta D) || b == (Movimenta C) || b == (Movimenta B) || b == (Movimenta E)) = []
                  | otherwise = [a]
colocaList (x:xs) a b | ((b == (Movimenta D) || b == (Movimenta C) || b == (Movimenta B) || b == (Movimenta E)) && (length (x:xs)) >= 0) = (x:xs)
                      | otherwise = a:x:xs
-}
-- | Verifica o mapa para saber se as 2 pecas a frente, consoante a direcao, estao livres
verificaMapa :: Jogada -> PosicaoGrelha -> Mapa -> Bool
verificaMapa (Movimenta D) (x,y) m | ((encontraPosicaoMatriz (x+1,y+2) m) == Vazia ) && ((encontraPosicaoMatriz (x+2,y+2) m) == Vazia) = True
                                   | otherwise = False
verificaMapa (Movimenta C) (x,y) m | ((encontraPosicaoMatriz (x,y) m) == Vazia ) && ((encontraPosicaoMatriz (x,y+1) m) == Vazia) = True
                                   | otherwise = False
verificaMapa (Movimenta B) (x,y) m | ((encontraPosicaoMatriz (x+3,y) m) == Vazia ) && ((encontraPosicaoMatriz (x+3,y+1) m) == Vazia) = True
                                   | otherwise = False
verificaMapa (Movimenta E) (x,y) m | ((encontraPosicaoMatriz (x+1,y-1) m) == Vazia ) && ((encontraPosicaoMatriz (x+2,y-1) m) == Vazia) = True
                                   | otherwise = False


-- | Alguns Testes
m = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel]
    ,[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

ds = [disparo1,disparo2,disparo3,disparo4]
disparo1 = DisparoCanhao 0 (3,1) B
disparo2 = DisparoLaser  1 (5,2) D
disparo3 = DisparoCanhao 2 (6,4) E
disparo4 = DisparoChoque 3 2

js = [jogador1,jogador2,jogador3,jogador4]
jogador1 = Jogador (2,5) B 10 1 0
jogador2 = Jogador (5,1) E 2 0 2
jogador3 = Jogador (7,7) E 0 2 2
jogador4 = Jogador (1,5) C 3 0 0

jpsyco = [jogador1]

a = (Estado m js ds)

ys = []