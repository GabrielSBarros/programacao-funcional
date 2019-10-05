module MultisetList ()
 where

{- 
 - Um multi-conjunto (ou bag) é uma estrutura que representa uma coleção de objetos que 
 - permite duplicadas. Entretanto, as duplicatas são armazenadas como a quantidade de 
 - ocorréncias do mesmo elemento no multi-conjunto. Exemplo, a coleção {a,b,c,c,c,b} poderia 
 - ser representada como sendo {(a,1), (b,2), (c,3)}. A ideia de multi-conjunto pode ser 
 - implementada de diversas formas. Uma delas é usando a implementacao de Data.List, onde 
 - cada elemento da lista consiste do dado em si e sua quantidade (um par). 
 - Eh recomendavel que voce consulte a documentacao de Data.List
 -}
import qualified Data.List as List

{-
 - Insere um elemento na estrutura. Caso o elemento ja existe, sua quantidade na estrutura sera incrementada.
 -}
insert elem [] = [(elem, 1)]
insert elem ((x, quantity):xs)
  | elem == x = ((x, (quantity + 1)):xs)
  | otherwise = ((x,quantity):insert elem xs)

{-
- Remove um elemento da estrutura, levando em consideracao a manipulacao de sua quantidade na estrutura. 
- Caso a quantidade atinja 0 (ou menos), o elemento deve realmente ser removido da estrutura
-}
remove elem [] = []
remove elem ((x, quantity):xs)
  | elem == x && quantity == 1 = xs
  | elem == x = ((x, quantity - 1):xs)
  | otherwise = ((x, quantity):remove elem xs)

{-
 - Busca um elemento na estrutura retornando sua quantidade. Caso o elemento nao exista, retorna 0 como a quantidade.
-}
search _ [] = 0
search elem ((x, quantity):xs)
  | elem == x = quantity
  | otherwise = search elem xs

{-
 - Faz a uniao deste Bag com otherBag. A uniao consiste em ter os elementos dos dois Bags com suas maiores quantidades.
 - Por exemplo, A = {(a,1),(c,3)}, B = {(b,2),(c,1)}. A.union(B) deixa A = {(a,1),(c,3),(b,2)}
-}
union [] bag2 = bag2
union ((x, quantity):xs) bag2
  | quantity2 > 0 && quantity2 > quantity = [(x, quantity2)] ++ union xs filteredBag
  | otherwise = [(x, quantity)]++ union xs filteredBag

    where quantity2 = search x bag2
          filteredBag = (filter (/=(x, quantity2)) bag2)


{-
 - Faz a intersecao deste Bag com otherBag. A intersecao consiste em ter os elementos que estao em ambos os bags com suas 
 - menores quantidades. Por exemplo, Seja A = {(a,3),(b,1)} e B = {(a,1)}. Assim, A.intersection(B) deixa A = {(a,1)}
 - Caso senhum elemento de A esteja contido em B ent�o a intersecao deixa A vazio.
-}
intersection [] _ = []
intersection ((x, quantity):xs) bag2
  | quantity2 > 0 = [(x, min quantity quantity2)] ++ intersection xs bag2
  | otherwise = intersection xs bag2
  where quantity2 = search x bag2
{-
 - Faz a diferenca deste Bag com otherBag. A diferenca A \ B entre bags eh definida como segue:
   - contem os elementos de A que nao estao em B
   - contem os elementos x de A que estao em B mas com sua quantidade subtraida (qtde em A - qtde em B). 
     Caso essa quantidade seja negativa o elemento deve serremovido do Bag. 
     Por exemplo, seja A = {(a,3),(b,1)} e B = {(b,2),(a,1)}. Assim, A.minus(B) deixa A = {(a,2)}.
-}
minus [] _ = []
minus ((x, quantity):xs) bag2
  | quantity2 < quantity = [(x, quantity - quantity2)] ++ minus xs bag2
  | otherwise = minus xs bag2

  where quantity2 = search x bag2
{-
 - Testa se este Bag esta incluso em otherBag. Para todo elemento deste bag, sua quantidade
 - deve ser menor or igual a sua quantidade em otherBag.
-}
inclusion [] _ = True
inclusion ((x, quantity):xs) bag2
  | quantity <= quantity2 = inclusion xs bag2
  | otherwise = False
  where quantity2 = search x bag2

{-
 - Realiza a soma deste Bag com otherBag. A soma de dois bags contem os elementos dos dois bags com suas quantidades somadas. 
-}
sumBag [] bag2 = bag2
sumBag ((x, quantity):xs) bag2
  | quantity2 > 0 = [(x, quantity + quantity2)] ++ sumBag xs filteredBag
  | otherwise = [(x, quantity)] ++ sumBag xs filteredBag

  where quantity2 = search x bag2
        filteredBag = (filter (/=(x, quantity2)) bag2)

{-
 - Retorna a quantidade total de elementos no Bag
-}
size [] = 0
size ((_, quantity):xs) = quantity + size xs