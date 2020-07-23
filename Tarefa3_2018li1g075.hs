
{-|
* Introducao à Tarefa:

    Esta tarefa tem como objetivo, consoante uma descrição do estado do jogo, comprimir ou descomprimir este mesmo,
    de forma a ser possível gravar o jogo e, num próximo momento, retomar do mesmo ponto. O problema deve ser abordado
    de forma a usar o menor número de carateres e, consequentemente, poupar espaço no disco. Foram feitos testes com vários estados
    e com várias strings, com resultados válidos. 

* Estratégia utilizada e resultados obtidos. 

    Para comprimir o estado do jogo, a solução implementada para esta tarefa, consiste em:
    
    * Comprimir o jogador numa string.
    
    * Comprimir a lista de jogadores numa string.
    
    * Comprimir o mapa numa string.
    
    * Comprimir o disparo numa string.
    
    * Comprimir a lista de disparos numa string.

    Quanto à descompressão, podemos considerar a solução implementada, como um inverso da compressão,
    abordando o problema da seguinte forma:
    
    * Descomprimir a string respetiva ao mapa (lista de listas de Pecas).
    
    * Descomprimir a string respetiva à lista de jogadores.
    
    * Descomprimir a string respetiva à lista de disparos.
    
    * Quando tudo estiver descomprimido, a funcao principal faz uso de todas as funcoes para devolver um estado.
    
    Após vários testes, os resultados obtidos apresentam-se corretos, e correspondem ao Estado dado.

* Conclusão

    Podemos então concluir que o problema levantado nesta Tarefa foi solucionado, apesar de não apresentar uma
    solução ideal, pois o número de carateres pode ainda ser mais reduzido.

Este módulo define funções comuns da Tarefa 3 do trabalho prático.
-}
module Tarefa3_2018li1g075 where

import LI11819
import Tarefa2_2018li1g075



-- * Testes
-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [a]

-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -> String
comprime (Estado a b c) = (comprimeA a) ++ "/" ++ (comprimeB b) ++ "/" ++ (comprimeC c)

-- | Comprime uma lista de pecas, numa string
comprimePeca :: [Peca] -> String 
comprimePeca [x] = (converte (show x)) 
comprimePeca (x:xs) = converte (show x) ++ comprimePeca xs

-- | Comprime o mapa numa string
comprimeA :: Mapa -> String
comprimeA (xs:[]) = comprimePeca xs ++ ""
comprimeA ((m):y) = (comprimePeca m)  ++ (comprimeA y)

-- | Comprime um jogador numa string
comprimeJogador :: Jogador -> String
comprimeJogador (Jogador a b c d e) = show (fst a) ++ " " ++ show (snd a) ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d ++ " " ++ show e

-- | Comprime a lista de jogadores numa string
comprimeB :: [Jogador] -> String
comprimeB [x] = comprimeJogador x ++ ""
comprimeB (x:xs) = comprimeJogador x ++ "." ++ (comprimeB xs)

-- | Comprime um disparo numa string
comprimeDisparo :: Disparo -> String
comprimeDisparo (DisparoCanhao a b c) = converteDisp (DisparoCanhao a b c) ++ " " ++ show a ++ " " ++ show (fst b) ++ " " ++ show (snd b) ++ " " ++ show c
comprimeDisparo (DisparoLaser a b c) = (converteDisp (DisparoLaser a b c)) ++ " " ++ show a ++ " " ++ show (fst b) ++ " " ++ show (snd b) ++ " " ++ show c
comprimeDisparo (DisparoChoque a b) = (converteDisp (DisparoChoque a b)) ++ " " ++ show a ++ " " ++ show b

-- | Comprime uma lista de Disparos numa String
comprimeC :: [Disparo] -> String
comprimeC [x] = comprimeDisparo x ++ ""
comprimeC (x:xs) = comprimeDisparo x ++ "." ++ comprimeC xs

-- | Converte o tipo de peca numa string
converte :: String -> String
converte "Bloco Indestrutivel" = "a"
converte "Bloco Destrutivel" = "b"
converte "Vazia" = "c"

-- | Converte o tipo de Disparo numa String
converteDisp :: Disparo -> String
converteDisp (DisparoCanhao a v x) = "d"
converteDisp (DisparoLaser a v x) = "e"
converteDisp (DisparoChoque a b) = "f" 


-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -> Estado
descomprime ls =  (Estado (descomprimeM (take(conta 0 ls) ls) (tamanho (take(conta 0 ls) ls) 0)) (main2(bruh(take (conta 0 (drop ((conta 0 ls)+1) ls)) (drop ((conta 0 ls)+1) ls) ))) (mainD(bruh(stringD))))
                     where
                        stringD = (drop((conta 0 (drop ((conta 0 ls)+1) ls) )+1) (drop ((conta 0 ls)+1) ls))

-- | Devolve o numero de colunas de um mapa em formato string
tamanho ::  String  -> Int -> Int
tamanho (x:xs) a = if x== 'a' then tamanho xs a+1 else (a-1)

-- | Conta o numero de carateres ao char "/", que e a separaco entre mapa/jogador/disparo
conta :: Int -> String -> Int
conta x (h:hs)= if h /= '/' then conta (x+1) hs else x 

-- | Converte um Char na respetiva Peca
inverte :: Char -> Peca
inverte 'a' = Bloco Indestrutivel 
inverte 'b' = Bloco Destrutivel
inverte 'c' = Vazia

-- | Funcao principal para descomprimir o mapa
descomprimeM :: String -> Int -> Mapa
descomprimeM [] _ = []
descomprimeM ls x = map inverte (take x ls) : descomprimeM (drop x ls) x


--descomprime jogador 

-- funcao final de cada jogador

-- | Recebe uma lista de Strings com cada string correspondente a cada jogador, convertendo a Jogador 
descomprimeJ ::  [String] -> Jogador  
descomprimeJ [] = Jogador (0,0) D 0 0 0 
descomprimeJ (a:b:c:d:e:[f]) = (Jogador (((read a) :: Int),(read b) :: Int) ((read c) :: Direcao) ((read d) :: Int) ((read e) :: Int) ((read f) :: Int))

-- | Recebe a string sem divisoes e devolve o Jogador
aux :: String -> Jogador
aux l = descomprimeJ (words l)

-- | Recebe uma lista de strings com os jogadores divididos e utiliza a map e a aux em cada Jogador da lista
descomprimeJs :: [String] -> [Jogador]
descomprimeJs l = map aux l

-- | Recebe o Eestado em String e divide numa lista de Strings com jogadores
bruh :: String -> [String]
bruh [] = [] 
bruh x = (take(contaJ x 0) x) : bruh (drop((contaJ x 0)+1)x)

-- | Conta os caracteres de cada jogador
contaJ :: String -> Int -> Int  
contaJ [] a = a 
contaJ (x:xs) a | x /= '.' = contaJ xs (a+1)
                | otherwise = a


-- | Recebe uma lista de strings com cada jogador, e devolve uma lista de Jogadores
main2 :: [String] -> [Jogador]
main2 [] = []
main2 l = descomprimeJs l

-- | Alguns testes
j = "aaaaaaaaaaaccacccccaaccacccccaaccacccccaaccacccccaaccccbbbbaaccacccccaaccccccccaaccccccccaaaaaaaaaaa/1 1 B 2 1 0.5 1 E 1 0 2.6 5 E 0 2 2/d 0 3 1 B.e 1 5 2 D.d 2 6 4 E"

-- | Alguns testes
v = "aaaaaaaaaaaccacccccaaccacccccaaccacccccaaccacccccaaccccbbbbaaccacccccaaccccccccaaccccccccaaaaaaaaaaa/1 1 B 2 1 0.5 1 E 1 0 2.6 5 E 0 2 2.1 5 C 3 0 0/d 0 3 1 B.e 1 5 2 D.d 2 6 4 E.f 3 2"

 
-- | Recebe a lista de Strings com disparos diferentes, e traduz para Disparo

descomprimeD:: [String] -> Disparo 
descomprimeD [] = DisparoCanhao 0 (0,0) E
descomprimeD (x:xs) |  (x == "d") = auxC (x:xs)
                    |  (x == "e") = auxL (x:xs) 
                    |  (x=="f") = auxF (x:xs) 

-- | Descomprime disparo canhao
auxC :: [String] -> Disparo
auxC (a:b:c:d:[e]) = DisparoCanhao ((read b) :: Int) ((read c) :: Int,(read d) :: Int) ((read e) :: Direcao)

-- | Descomprime disparo laser
auxL :: [String] -> Disparo
auxL (a:b:c:d:[e]) = DisparoLaser ((read b) :: Int) ((read c) :: Int,(read d) :: Int) ((read e) :: Direcao)

-- | Descomprime disparo Choque 
auxF :: [String] -> Disparo
auxF (a:b:[c]) = DisparoChoque ((read b) :: Int) ((read c) :: Ticks)

-- | Pega na lista de Strings com cada disparo e traduz para uma lista de disparos
mainD :: [String] -> [Disparo]
mainD [] = []
mainD (x:xs) = map aux' (x:xs)

-- | Traduz um disparo em string para um disparo 
aux' :: String -> Disparo
aux' l = descomprimeD (words l)



-- * Conclusão
-- |



