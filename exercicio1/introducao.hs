{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = (a && (not b)) || ((not a) && b)
impl a b = not a || b
equiv a b = impl a b && impl b a

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y = x * pow x (y - 1)


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x = x * fatorial (x - 1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime 1 = True
isPrime 2 = True
isPrime x = (length [y | y <- [2..x], mod x y == 0]) < 2

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}

mdc x 0 = x
mdc x y 
    | x > y = mdc y (x - y)
    | otherwise = mdc x (y - x)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = head [ z | z <- [x..y*x], mod z x == 0, (mod z y) == 0]

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = (mdc x y == 1)

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
primosAte x = [z | z <- [1..x], isPrime z]

goldbach x 
    | x < 2 = []
    | mod x 2 /= 0 = []
    | otherwise = [(y, z) | y <- primosAte x, z <- primosAte x, y + z == x, y <= z]
