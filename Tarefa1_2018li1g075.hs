-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g075 where

import LI11819
import Tarefa0_2018li1g075
-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[MudaTetromino,Move B,Move D,Roda,Move E,Move B,MudaParede,Move D,Desenha,Move C],[Move B,Move D,Move D,Move C,Move E,Move E,Desenha,Move D,Roda,Move D,Move B,Move D,Desenha,Move B,MudaParede,Desenha],[Move B,Move D,Move D,Move C,Move E,Move E,Desenha],[Move B,Move B,Move B,Move D],[Move B,Move D,Move B,Move D,Move B,Move D,Move B,Move D],[Desenha],[Desenha,Move B,Move D,Move D,Desenha,Move D,Move B,Move D,Roda,MudaTetromino,MudaParede,Desenha]]

-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.

-- | A funcao mudaParede recebe uma parede, e altera para a unica parede alternattiva

mudaParede :: Parede -> Parede
mudaParede  Destrutivel = Indestrutivel
mudaParede  Indestrutivel = Destrutivel

-- | A funcao mTetromino altera o Tetronimo atual pelo proximo

mTetromino ::  Tetromino -> Tetromino
mTetromino x | x == I = J
             | x == J = L
             | x == L = O
             | x == O = S
             | x == S = T
             |x == T = Z
             | x == Z = I



--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
 -- ^ A 'Instrucao' a aplicar.
              -- ^ O 'Editor' anterior.
            -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao :: Instrucao -> Editor -> Editor -- ^ A 'Instrucao' a aplicar.-- ^ O 'Editor' anterior.-- ^ O 'Editor' resultante após aplicar a 'Instrucao'.
instrucao (Move x) (Editor a b c d e) = (Editor (somaVetores a (direcaoParaVetor x)) b c d e)
instrucao (Roda) (Editor a b c d e) ={-(existe(verifica (par (0,0) (atualizaD c (roda90 b))) e))-}  (Editor a (roda90 b) c d e)
                                   -- | otherwise = (Editor a  b c d e)
instrucao MudaTetromino (Editor a b c d e) = (Editor a b (mTetromino c) d e)
instrucao MudaParede (Editor a b c d e) = (Editor a b c (mudaParede d) e)
instrucao Desenha (Editor a b c d e) = (Editor  a b c d (mudaMapa (Bloco d) (map (somaVetores a) (ret (atualizaD c b))) e))


-- | Recebe uma Peca, uma lista de Posicoes e um mapa. Retrona o mapa atualizado consoante as pecas novas

mudaMapa :: Peca -> [Posicao] -> Mapa -> Mapa
mudaMapa x _ []=[]
mudaMapa a [] l = l
mudaMapa a (h:t) aux = mudaMapa a t (atualizaPosicaoMatriz h a aux)


-- | Atualiza uma matriz de Tetrominos consoante a direcao
atualizaD :: Tetromino->Direcao->Matriz Bool
atualizaD x y | y == C = tetrominoParaMatriz x
              | y == D = rodaMatriz (tetrominoParaMatriz x)
              | y == B =rodaMatriz(rodaMatriz(tetrominoParaMatriz x))
              | y == E =  rodaMatriz(rodaMatriz(rodaMatriz(tetrominoParaMatriz x))) 


-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -> Editor -> Editor -- ^ As 'Instrucoes' a aplicar.-- ^ O 'Editor' anterior.-- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] x = x
instrucoes (x:xs) a = (instrucoes xs (instrucao x a)) 

  --Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (x,y) = (mapa (x,y) 0)


-- | Gera a linhas interiores de um mapa Vazio
linhainterior :: Int -> Int -> [Peca]
linhainterior x  a | (a == 0) || (a== (x-1)) = (Bloco Indestrutivel) : linhainterior x (a+1)
                   | (a == x) = []
                   | otherwise = Vazia : linhainterior x (a+1)

-- | Gera a primeira e ultima linha
blocoI :: Int -> [Peca]
blocoI x = (replicate x (Bloco Indestrutivel))


-- |Consoante uma dimensao e um contador para as linhas, gera um mapa Vazio
mapa :: Dimensao -> Int -> Mapa 
mapa (x,y) a | (a == 0) = (blocoI y) : (mapa (x,y) (a+1))
             | (a== (x-1)) = [(blocoI y)] 
             |otherwise = (linhainterior y 0) : mapa (x,y) (a+1)
-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.

editorInicial :: Instrucoes -> Editor  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.-- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial x = (Editor (posicaoInicial x) (C) (I) (Indestrutivel) (mapaInicial (dimensaoInicial x)))

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -> Mapa  -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.     
constroi x =  aux (instrucoes x  (editorInicial x))  

-- |Print a um mapa do Editor
aux :: Editor -> Mapa
aux (Editor a b c d e)= e

-- |Roda a direcao 90ª
roda90 :: Direcao -> Direcao
roda90 C = D
roda90 D = B
roda90 B = E
roda90 E = C

-- | Dado um contador de posicoes e uma matriz de Bool (Tetromino), gera a lista de posicoes a que pertence o tetromino (True)
par :: Posicao -> Matriz Bool -> [Posicao]
par _ [[]]= [] 
par (x,y) ((h:[]):t)  | h  && (t /= []) = (x,y):par(x+1,0) t
                      | t == [] && (h) = (x,y) : []
                      | (not h) && (t/= []) = par (x+1,0) t 
                      | otherwise = []               
par (x,y) ((h:t1):t2) | h = (x,y): par (x,y+1) (t1:t2)
                      | otherwise = par (x,y+1) (t1:t2)

-- | diferencia o caso especifico da matriz de dimensao (2,2) se x for a uma matriz com 2 linhas gera o quadrado, otherwise utiliza a funcao par definida acima
ret :: Matriz Bool -> [Posicao]
ret x | (length x == 2) = [(0,0),(0,1),(1,0),(1,1)]
      | otherwise = par (0,0) x                      

