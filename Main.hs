-- Aluno: Carlos Henrique Moreira dos Santos
import Text.Printf

{--
1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. 
--}
divisoresden :: Int -> [Int]
divisoresden x = [y | y <- [1..x-1], x `mod` y == 0]

{--
2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a ocorrência de um caractere específico, em uma string dada. 
--}
contaCaractere :: String -> Char -> Int
contaCaractere x y = length [z | z <- x, z == y]


{--
3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. 
--}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo lista = [x*2 | x <- lista, x>0]

{--
4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. 
--}
pitagoras :: Int -> [(Int,Int,Int)]
pitagoras x = [(a,b,c) | a <-[1..x], b <-[1..x], c <- [1..x], ((a^2) + (b^2)) == (c^2), c > a, c > b, b > a]

{--
5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. 
--}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos x = [y | y <- [1..x], sum (divisoresden y) == y]

{--
6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no prelude que podem ser úteis. 
--}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar lista1 lista2 = (sum [i*j | (i, j) <- zip lista1 lista2])

{--
7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2. 
--}
primeirosPrimos :: Int -> [Int]
primeirosPrimos x = [y | y <- [2..x], length (divisoresden y) == 1]

{--
8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. 
--}
paresOrdenados :: Int -> [(Int,Int)]
paresOrdenados x = [(2^a,3^a) | a <- [1..x]]


main = do
  --Ex1
  printf "\nFunc. 1: entrada:%d; resultado:%s\n" (20::Int) (show (divisoresden 20))

  --Ex2
  printf "\nFunc. 2: entrada:%s %s; resultado:%s\n" (show ("arara")) (show ('a')) (show (contaCaractere "arara" 'a'))

  -- Ex3
  printf "\nFunc. 3: entrada:%s; resultado:%s\n" (show [1,2,3,4,-5,6-7,10,-203]) (show (dobroNaoNegativo [1,2,3,4,-5,6-7,10,-203]))

  --Ex4
  printf "\nFunc. 4: entrada:%s; resultado:%s\n" (show 20) (show (pitagoras 20))

  --Ex5
  printf "\nFunc. 5: entrada:%d; resultado:%s\n" (500::Int) (show (numerosPerfeitos 500))

  --Ex6
  printf "\nFunc. 6: entrada:%s %s; resultado:%d\n" (show [2,3,4,5]) (show [6,7,8,9]) (produtoEscalar [2,3,4,5] [6,7,8,9])

  --Ex7
  printf "\nFunc. 7: entrada:%d; resultado:%s\n" (100::Int) (show (primeirosPrimos 100))

  --Ex8
  printf "\nFunc. 8 FLIP 1: entrada:%d; resultado:%s\n" (10::Int) (show (paresOrdenados 10))

  
  
