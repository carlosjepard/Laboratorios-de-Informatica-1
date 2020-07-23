
{-|
* Introdução à Tarefa:

    Esta tarefa tem como finalidade realizar a parte gráfica
    do jogo, sendo para isso, utilizada a biblioteca Gloss. Esta tarefa divide-se em duas partes.
    A primeira parte consiste em apresentar o jogo no ecrã, sendo esta apresentação sempre fiél ao estado do jogo.
    Na segunda parte, é pretendido dar ao jogador a possiblidade de alterar o estado do jogo.
    
* Estratégia utilizada e resultados obtidos. 

    A estratégia utilizada consiste em converter o mapa, a lista de jogadores e
    a lista de disparos, numa lista de pictures, com as posições associadas.
    Usando a função Translate, as imagens serão apresentadas
    na janela, seguindo uma relação que, a partir das coordenadas (peças, tanques, tiros), desenha 
    essas mesmas imagens no local correto, no ecrã. 
    Na segunda parte, foi feita a implementação de um função que
    atualiza o estado do jogo e outra que, chamando recursivamente essa
    função, reage a um evento.
    Vários testes foram feitos, sendo que o mapa correspondeu sempre ao mapa dado, porém,
    quanto à jogabilidade, foram detetadas algumas falhas. Alguns mapas apresentam-se
    descordenados em relação à posição dos tanques, sendo que estes mesmos, estão cercados 
    pelo mapa "invisivel".

    Algumas imagens de mapas
    
    <<images/imagem2.png imagem1>> <<images/imagem1.png imagem2>>
  
* Conclusão
    
    Após vários testes, é possível concluir que, o programa realiza a apresentação
    do mapa, sendo esta válida. No entanto, é de salientar que, sendo o jogo Tanks jogável,
    algumas funcionalidades não estão completamente funcionais.

    
Este módulo define funções comuns da Tarefa 5 do trabalho prático.
-}

module Main where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Tarefa3_2018li1g075 
import Tarefa2_2018li1g075
import Tarefa4_2018li1g075
import LI11819

-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
main :: IO ()
main = do inicio <- estadoInicial
          joga inicio 

-- | Definicção do Estado Gloss
type Estado2 = (Estado,Float,Picture,Picture,Picture,Picture,Picture,Picture,Picture,Picture)


-- | funcao que implementa a jogabilidade em geral
joga :: Estado2 -> IO ()
joga inicio = play
    window
    (black)                               
    fr                                      
    inicio                                    
    desenhoEstado  
    reageEvento
    reageTempo                                 

-- | Estado inicial do jogo. Contem o primeiro estado
estadoInicial :: IO Estado2
estadoInicial = do 
  pedraDest    <- loadBMP "imagens/pedraDest.bmp"
  pedraInds    <- loadBMP "imagens/pedraInds.bmp"
  vazio        <- loadBMP "imagens/vazio.bmp"
  tanque       <- loadBMP "imagens/tanque.bmp"
  laser        <- loadBMP "imagens/laser.bmp"
  choque       <- loadBMP "imagens/choque.bmp"
  canhao       <- loadBMP "imagens/canhao.bmp"
  gameover     <- loadBMP "imagens/gameover.bmp"  
  return (ex1,80,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) 

-- | Exemplo de um estado
ex1 :: Estado
ex1=(Estado ms js ys)

-- | Funcao que calcula o numero de linhas do mapa

linhas :: Estado2 -> Int
linhas ((Estado (x:xs) hs ys),a,b,c,d,e,f,g,h,i) = length (x)

-- | Funcao que calcula o numero de colunas no mapa

colunas :: Estado2 -> Int
colunas ((Estado (x:xs) hs ys),a,b,c,d,e,f,g,h,i) = length (x:xs)


-- | Funcao que vai converter uma lista de listas de peças (mapa) numa lista de Pecas
matriz_para_lista :: Mapa -> [Peca]
matriz_para_lista [] = [] 
matriz_para_lista [x,[]] = x 
matriz_para_lista [l] = l
matriz_para_lista (h:t) = h ++ (matriz_para_lista t)


-- | Funcao que recebe um estado e converte numa lista de imagens
tentarDesenhar :: Estado -> [Imagens] 
tentarDesenhar (Estado xs hs ys) = aux (matriz_para_lista (reverse xs))
          where aux [] = [] 
                aux (h:t) | (h == Bloco Destrutivel) = PedraDest : aux t 
                          | (h == Bloco Indestrutivel) = PedraInds : aux t
                          | (h == Vazia) = Vazio2 : aux t 

-- | Conjunto de imagens, para facilitar a conversao para Pictures                          
data Imagens =  PedraInds| PedraDest | Vazio2
  deriving (Show,Eq)


-- | Funcao que a partir de um estado Gloss, cria uma lista de Pictures
tentarDesenhar2 :: Estado2 -> [Picture]
tentarDesenhar2 (estado,_,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) = aux1 (tentarDesenhar estado)
                       where aux1 [] = []
                             aux1 (h:t) | h == PedraInds = (pedraInds : aux1 t) 
                                        | h == PedraDest = (pedraDest : aux1 t) 
                                        | h == Vazio2 = (vazio : aux1 t)

-- | Funcao que transforma um estado Gloss numa lista de listas de Pictures
outramatriz :: Estado2 -> [[Picture]]
outramatriz estado2 = aux3 (linhas estado2) (tentarDesenhar2 (estado2))

-- | Funcao de auxilio a funcçao "outramatriz"
aux3 :: Int -> [Picture] -> [[Picture]]
aux3 x [] = []
aux3 x list = [take x list] ++ (aux3 x (drop x list)) 

-- | Funcao de auxilio à funcao "matriz_final"
matriz2 :: [[Picture]] -> (Int,Int) -> [(Picture,(Int,Int))]
matriz2 [] (x,y) = []
matriz2 (h:t) (x,y) = (aux (x,y) h) ++ matriz2 t (x,y+1)
          where aux (x,y) [] = []
                aux (x,y) (h:t) = (h,(x,y)) : aux (x+1,y) t

-- | Funcao que, ao longo de uma lista de listas de Pictures, lhe vai atribuir as posicoes de forma a facilitar o Translate
matriz_final :: [[Picture]] -> [(Picture,(Int,Int))]
matriz_final pics = matriz2 pics (0,0)  

-- | Funcao que devolve uma lista de Pictures, com cada peca com as coordenadas definidas
desenhaMapa :: [(Picture,(Int,Int))] -> Estado2 -> [Picture]
desenhaMapa [] e = []
desenhaMapa ((x,(x1,y1)):t) e = (Translate (fromIntegral ((x1) * 256)) (fromIntegral ((y1)*256)) x) : desenhaMapa t e
                  
-- | Funcao que desenha o Estado Gloss dado
desenhoEstado :: Estado2 -> Picture
desenhoEstado e@(estado,a,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)| a <= 0 =(Pictures [(Translate (0) (0) gameover)])
desenhoEstado e  = Scale (0.2) (0.2) (Translate (funcao_Translatex e) (funcao_Translatey e) (Pictures (((desenhaMapa (matriz_final (outramatriz e)) e)) ++ (tentarDesenhar4 e (tentarDesenhar3 e)) ++ (canhao_final e (desenha_tiro e))))) 



-- | Funcao que assegura que o mapa se encontra no centro da janela, e nao vai para alem das bordas da janela
funcao_Translatex :: Estado2 -> Float
funcao_Translatex e = (fromIntegral(linhas(e)) * (-100))

-- | Funcao que assegura que o mapa se encontre no centro da janela, e nao vai alem das bordas da janela
funcao_Translatey :: Estado2 -> Float
funcao_Translatey e = (fromIntegral (colunas(e)) * (-100))

-- | Funcao que apresenta a janela que vai apresentar o jogo
window :: Display 
window = InWindow "tanks" (800,800) (0,0)

-- | Frame Rate do jogo
fr :: Int
fr = 10 

-- | Funcao que recebe o faz com que o jogo reaga ao tempo
reageTempo :: Float -> Estado2 -> Estado2
reageTempo x (estado,tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) = ((tick estado),(tempo-x),pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)

-- | Funcao que faz o tank reagir aos movimentos, consoante a tecla pressionada
reageEvento :: Event -> Estado2 -> Estado2
reageEvento (EventKey (SpecialKey KeyUp)    Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Movimenta C) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                               | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (SpecialKey KeyDown)  Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Movimenta B) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                               | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) 
reageEvento (EventKey (SpecialKey KeyLeft)  Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Movimenta E) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                               | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Movimenta D) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                               | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'j') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Dispara Canhao) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'k') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Dispara Laser) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
--reageEvento (EventKey (Char 'l') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Dispara Choque) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
--                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'w') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 1 (Movimenta C) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'a') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 1 (Movimenta E) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 's') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 1 (Movimenta B) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'd') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 1 (Movimenta D) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'z') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 1 (Dispara Canhao) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento (EventKey (Char 'x') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 1 (Dispara Laser) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
--reageEvento (EventKey (Char 'c') Down _ _) (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (t>0) = ((jogada 0 (Dispara Choque) estado),t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
--                                                                                                                    | otherwise = (estado,t,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
reageEvento _ a = a -- ignora qualquer outro evento

-- | Funcao de que recebe um estado gloss e devolve uma lista com de pictures dos tanks em jogo, com a posicao respetiva
tentarDesenhar3 :: Estado2 -> [(Picture,(Float,Float))]
tentarDesenhar3 ((Estado m [] d),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) = []
tentarDesenhar3  ((Estado m ((Jogador (x,y) b c d e):xs) disparo),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) | (c <= 0) = [] ++ tentarDesenhar3 ((Estado m xs disparo),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
 | otherwise = [(Rotate (rotacao b) tanque,((fromIntegral (y)) +5.5,(fromIntegral (x))-2.5))] ++ tentarDesenhar3 ((Estado m xs disparo),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)

-- | Funcao que recebe um estado Gloss e uma lista de Pictures com as respetivas posicoes, resultando o translate dessas imagens
tentarDesenhar4 :: Estado2 -> [(Picture,(Float,Float))] -> [Picture] 
tentarDesenhar4 e [] = []
tentarDesenhar4 e ((tanque,(x,y)):t) =  (Translate (((x-c) * 256)) (((c-y)*256)) tanque) : tentarDesenhar4 e t 
                                           where c = fromIntegral (div (colunas e) 2)
                                            


-- | Funcao que roda a imagem, consoante a Direcao 
rotacao :: Direcao -> Float
rotacao x | x == E = 90
          | x == C = 180
          | x == D = 270
          | x == B = 0

-- | Funcao de que recebe um estado gloss e devolve uma lista com de pictures dos tiros em jogo, com a posicao respetiva
 
desenha_tiro :: Estado2 -> [(Picture,(Float,Float))]
desenha_tiro ((Estado m j []),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) = []
desenha_tiro ((Estado m j ((DisparoCanhao a (x,y) z):xs)),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) = [(Rotate (rotacao z) canhao,((fromIntegral y) + 5.5,(fromIntegral x)-2.5))] ++ desenha_tiro ((Estado m j xs),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)  
desenha_tiro ((Estado m j ((DisparoLaser a (x,y) z):xs)),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) = [(Rotate (rotacao z) laser,((fromIntegral y) + 5.5,(fromIntegral x)-2.5))] ++ desenha_tiro ((Estado m j xs),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover)
--desenha_tiro ((Estado m j ((DisparoChoque a b):xs)),tempo,pedraDest,pedraInds,vazio,tanque,laser,choque,canhao,gameover) =   

-- | Funcao que recebe um estado Gloss e uma lista de Pictures com as respetivas posicoes, resultando o translate dessas imagens
canhao_final :: Estado2 -> [(Picture,(Float,Float))] -> [Picture]
canhao_final e [] = []
canhao_final e ((canhao,(x,y)):t) = (Translate (((x-c) * 256)) (((c-y)*256)) canhao) : canhao_final e t 
                              where c = fromIntegral (div (colunas e) 2)
canhao_final e ((laser,(x,y)):t) = (Translate (((x-c) * 256)) (((c-y)*256)) laser) : canhao_final e t 
                              where c = fromIntegral (div (colunas e) 2) 
