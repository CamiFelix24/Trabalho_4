{- Camila Przendziuk Franco Felix -}

import Data.Char

soma :: (Int, Int) -> Int
soma (x, y) = x + y

divide :: Float -> Float -> Float
divide x y = x / y

{- Questão 1. Escreva  uma  função  chamada  fatorialn  que  usando  o  operador  range  e  a  função  foldr devolva o fatorial de n. -}
fatorialn :: Int -> Int
fatorialn x = foldr (*) 1 [1..x]

{- Questão 2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos 
reais listados. -}
quadradoReal :: [Float] -> [Float]
quadradoReal [] = []
quadradoReal x = map (^2) x

{- Questão 3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras. -}
comprimentoPalavras :: [[a]] -> [Int]
comprimentoPalavras = map length 

{- Questão 4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29.  -}
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum (filter divisivel [0..99999])
  where divisivel x = (mod x 29) == 0

{- Questão 5. Usando  a  função  filter  escreva  uma  função,  chamada  maiorMultiploDe que  recebe  um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. -}
maiorMultiploDe :: Int -> Int
maiorMultiploDe x = maximum (filter divisivel [0..99999])
  where divisivel y = (mod y x) == 0

{- Questão 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠=1^2 +2^2 +3^2 +4^2...+𝑛^2. -}
somaQuadrados :: Int -> Int
somaQuadrados x = foldr (+) 0 (listaq x)
  where listaq x = map (^2) [1..x]

{- Questão 7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.  -}
comprimento :: [a] -> Int
comprimento = foldl (\n _ -> n + 1) 0

{- Questão 8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. -}


main = do
  putStrLn $ "\nFunc. 1: Entrada 10; Resultado: " ++ show (fatorialn 10)
  putStrLn $ "\nFunc. 2: Entrada [2, 0.6, 25, -162]; Resultado: " ++ show (quadradoReal [2, 0.6, 25, -162])
  putStrLn $ "\nFunc. 3: Entrada [funcao, de, comprimento, de, palavras]; Resultado: " ++ show (comprimentoPalavras ["funcao", "de", "comprimento", "de", "palavras"])
  putStrLn $ "\nFunc. 4: Entrada:Não recebe entrada; Resultado: " ++ show (maiorMultiploDe29)
  putStrLn $ "\nFunc. 5: Entrada 29; Resultado: " ++ show (maiorMultiploDe 29)
  putStrLn $ "\nFunc. 6: Entrada 5; Resultado: " ++ show (somaQuadrados 5)
  putStrLn $ "\nFunc. 7: Entrada [comprimento, de, uma, lista]; Resultado: " ++ show (comprimento ["comprimento", "de", "uma", "lista"])
  putStrLn $ "\n\nFunc. 8.flip (/): Entrada 2 4; Resultado: " ++ show (flip (/) 2 4)
  putStrLn $ "\nFunc. 8.flip (>): Entrada 7 9; Resultado: " ++ show (flip (>) 7 9)
  putStrLn $ "\n\nFunc. 8.ord: Entrada 'a' ; Resultado: " ++ show (ord 'a')
  putStrLn $ "\nFunc. 8.ord: Entrada /n ; Resultado: " ++ show (ord '\n')
  putStrLn $ "\n\nFunc. 8.max: Entrada 5 8; Resultado: " ++ show (max 5 8)
  putStrLn $ "\nFunc. 8.max: Entrada 15.7 3.4; Resultado: " ++ show (max 15.7 3.4)
  putStrLn $ "\n\nFunc. 8.min: Entrada 15 6; Resultado: " ++ show (min 15 6)
  putStrLn $ "\nFunc. 8.min: Entrada 2.3 7.5; Resultado: " ++ show (min 2.3 7.5)
  putStrLn $ "\n\nFunc. 8.curry fst: Entrada 3 8; Resultado: " ++ show (curry fst 3 8)
  putStrLn $ "\nFunc. 8.curry : Entrada 4 6; Resultado: " ++ show (curry soma 4 6)
  putStrLn $ "\n\nFunc. 8.uncurry mod: Entrada (5,4); Resultado: " ++ show (uncurry mod (5,4))
  putStrLn $ "\nFunc. 8.uncurry : Entrada 9 3; Resultado: " ++ show (uncurry divide (9, 3))