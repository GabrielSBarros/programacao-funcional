{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor :: Bool -> Bool -> Bool
xor a  b = ((a && (not b)) || ((not a) && b))

impl :: Bool -> Bool -> Bool
impl a b = (not a) || b

equiv:: Bool -> Bool -> Bool
equiv a b = ((not a) || b) && ((not b) || a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square :: Float -> Float
square x = x * x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow :: Float -> Float -> Float
pow x 0 = 1
pow x y 
    | y > 0 = x * pow x (y - 1)
    | y < 0 = (pow x (y + 1)) / x

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial x = x * fatorial (x - 1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = isPrimeAux x 2


isPrimeAux :: Integer -> Integer -> Bool
isPrimeAux x i
    | ((mod x i) /= 0) = isPrimeAux x (i + 1)
    | x == i = True
    | otherwise = False



{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2) 

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc :: Integer -> Integer -> Integer
mdc x 0 = x
mdc x y 
    | x > y = mdc y (x - y)
    | otherwise = mdc x (y - x)

{-
- Calcula um MMC de dois numeros. 
-}
divisivel x y = mod x y == 0

primosAte x = [z | z <- [1..x], isPrime z]

mmc ::  Integer -> Integer -> Integer
mmc x y = foldr (*) 1 (mmc' x y [z | z <- [1..(max x y)], isPrime z])

mmc' :: Integer -> Integer -> [Integer] -> [Integer]
mmc' x y [] = []
mmc' x y (z: zs)
    | divisivel x z || divisivel y z = (z : (mmc' x y zs))
    | otherwise =  (mmc' x y zs)

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo :: Integer -> Integer -> Bool
coprimo x y = mdc x y == 1

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach :: Integer -> [(Integer, Integer)]
goldbach x 
    | x < 2 = []
    | mod x 2 /= 0 = []
    | otherwise = [(y, z) | y <- primosAte x, z <- primosAte x, y + z == x, y <= z]
