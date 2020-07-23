{-|
* Introdução à Tarefa:

    A tarefa 6 tem como finalidade implementar um bot que jogue Tanks de forma autónoma 
    e eficiente.

* Estratégia utilizada e resultados obtidos. 

    O método usado consiste em fazer o bot tomar a melhor decisão, isto é,
    consoante o estado do jogo, o bot deve decidir se se deve movimentar ou se deve disparar.
    Caso algum jogador esteja na mesma linha ou na mesma coluna que o bot,e se este pela frente
    do bot, o bot dispara um dos disparos. Se eliminar o jogador, avanca.
    Caso contrário, o bot deve se movimentar para um dos lados.
    Após varios testes, é possivel observar que, em caso de encontrar um jogador, o bot toma uma decisão acertada,
    no entanto, em caso de movimento, foram verificadas algumas decosões erradas.
    Todas as decisões são tomadas seguindo uma ordem de relevancia consoante os casos 
    previamente descritos
    
* Conclusão
    
    Após verificar o resultado final, é correto afirmar que o bot toma algumas decisões corretas,
    no entanto, ainda se encontra num estado um pouco limitado, devido a, por vezes, tomar decisões 
    pouco acertadas. 

    
Este módulo define funções comuns da Tarefa 6 do trabalho prático.
-}

module Tarefa6_2018li1g075 where

import LI11819
import Tarefa2_2018li1g075

-- * Funções principais da Tarefa 6.


-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
   -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
   -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot a (Estado e ((Jogador (x,y) d n m b):xs) ls) = (moveB  (devolB (Estado e ((Jogador (x,y) d n m b):xs) ls) a) e ((Jogador (x,y) d n m b):xs) (Estado e ((Jogador (x,y) d n m b):xs) ls) a)
                                     


-- | funcao que devolve o bot 
devolB :: Estado -> Int -> Jogador
devolB (Estado e (x:xs) ls) a = if  a == 1 then x else devolB (Estado e xs ls) (a-1)

-- | função que recebe um Jogador, e devolve a sua posição
pos:: Jogador -> Posicao
pos (Jogador a b c d e) = a


-- | funcao principal que dado o jogador(bot) o mapa e a lista de jogadores (sem o bot ) gera uma jogada dependendo de varias variaves
moveB :: Jogador -> Mapa -> [Jogador] ->Estado-> Int -> Maybe Jogada
moveB (Jogador (a,b) d n m v)  map' (x:xs) ls z| (vlinha (a,b) (eli (a,b) (pega'(x:xs)) ) d) =  Just  (Dispara Canhao)
                                               | (vcoluna (a,b) (eli (a,b) (pega'(x:xs)) )d) =  Just  (Dispara Canhao)
                                               | (vlinhaD (a,b) (eli (a,b) (pega'(x:xs))) d) =  Just  (Dispara Canhao)
                                               | (vcolunaB (a,b) (eli (a,b) (pega'(x:xs)) )d) = Just (Dispara Canhao)
                                               | (disparaCho (a,b) (conver(eli (a,b) (pega'(x:xs)))))= Just (Dispara Choque)
                                               | (verificaMapa (Movimenta C) (pos(devolB ls z))  map') = Just (Movimenta C)
                                               | (verificaMapa (Movimenta D) (pos(devolB ls z)) map') = Just (Movimenta D)
                                               | (verificaMapa (Movimenta E) (pos(devolB ls z)) map') = Just (Movimenta E)
                                               | (verificaMapa (Movimenta B) (pos(devolB ls z)) map') = Just (Movimenta B)
                                               | otherwise = Just (Dispara Laser)



-- | funcao q faz o bot ativar o disparochoque, se  algum jogador estiver dentro do range do disparo, o bot ativa o disparo
disparaCho :: Posicao -> [Posicao] -> Bool 
disparaCho (x,y) [] = False
disparaCho (x,y) ((a,b):xs) |(sqrt((x'^2)+(y'^2))) <= sqrt((a'^2)-(b'^2)) = True 
                            |otherwise = disparaCho (x,y) xs
              where  x' = fromIntegral $ x
                     y' = fromIntegral $ y
                     a' = fromIntegral $ a
                     b' = fromIntegral $ b
-- | transforma a lista de jogadores do estado numa lista de posicoes
pega'::[Jogador] ->  [(Posicao,Int)]
pega' [] = []
pega' ((Jogador (x,y) d n b m):xs) = ((x,y),n): pega' xs
-- | pega na posicao do bot, na lista de posicoes e elimina a posicao do bot
eli :: Posicao ->[(Posicao,Int)] -> [(Posicao,Int)]
eli a [] = []
eli (x,y) (h:t) = if (x,y) ==(fst h) then eli (x,y) t else h: eli (x,y) t  


-- | funcao util para a funcao principal  porque a funcao que ativa os choques so trabalha com uma lista de posicoes
conver :: [(Posicao,Int)] -> [Posicao]
conver [] = []
conver ((x,y):xs)= x : conver xs

-- | verifica se algum jogador se encontra na mesma linha que o bot , e se encontra a esquerda, o bot dispara 
vlinha :: Posicao -> [(Posicao,Int)] -> Direcao -> Bool
vlinha (x,y)  [] d = False
vlinha (x,y) (((a,b),n):xs) d | (n==0) = False
                              | (x == a) && (b<y)  && (d== E) = True
                              | otherwise = vlinha (x,y) xs d 
-- | verifica se algum bot se encontra a direita do bot , e se está, este dispara
vlinhaD :: Posicao -> [(Posicao,Int)] -> Direcao -> Bool
vlinhaD (x,y)  [] d = False
vlinhaD (x,y) (((a,b),n):xs) d| n == 0 = False 
                              | (x == a) && (b>y)  && (d== D) = True
                              | otherwise = vlinhaD (x,y) xs d






-- | verifica se algum jogador se encontra na mesma coluna que o bot , e se encontra acima deste , o bot dispara
vcoluna :: Posicao -> [(Posicao,Int)] -> Direcao -> Bool 
vcoluna (x,y) [] d = False
vcoluna (x,y) (((a,b),n):xs) d | n == 0 = False
                               | (y == b) && (a<x) && (d==C) = True
                               | otherwise = vcoluna (x,y) xs d 

-- | verifica se algum jogador se encontra na mesma coluna que o bot ,  e se encontra abaixo dele, dispara 
vcolunaB :: Posicao -> [(Posicao,Int)] -> Direcao -> Bool 
vcolunaB (x,y) [] d = False
vcolunaB (x,y) (((a,b),n):xs) d | n == 0 = False
                                | (y == b) && (a>x) && (d==B) = True
                                | otherwise = vcolunaB (x,y) xs d 



