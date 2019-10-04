--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Tri a b c deriving (Eq,Show)

tripleFst (Tri a _ _) = a
tripleSnd (Tri _ b _) = b
tripleThr (Tri _ _ c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quad a a b b

firstTwo (Quad a b _ _)= (a, b) 
secondTwo (Quad _ _ c d)= (c, d) 

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d 

tuple1 (Tuple1 a) = Just a 
tuple1 (Tuple2 a _) = Just a 
tuple1 (Tuple3 a _ _) = Just a 
tuple1 (Tuple4 a _ _ _) = Just a 

tuple2 (Tuple1 _) = Nothing 
tuple2 (Tuple2 _ b) = Just b 
tuple2 (Tuple3 _ b _) = Just b
tuple2 (Tuple4 _ b _ _) = Just b

tuple3 (Tuple1 _) = Nothing 
tuple3 (Tuple2 _ _) = Nothing
tuple3 (Tuple3 _ _ c) = Just c
tuple3 (Tuple4 _ _ c _) = Just c

tuple4 (Tuple1 _) = Nothing 
tuple4 (Tuple2 _ _) = Nothing
tuple4 (Tuple3 _ _ _) = Nothing
tuple4 (Tuple4 _ _ _ d) = Just d

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isOrdered [] = True
isOrdered (x:[]) = True
isOrdered (x:y:xs)
  | x > y = False
  | otherwise = isOrdered (y:xs)


isBST node = isOrdered (order node)

--insere uma nova chave na BST retornando a BST modificada
insert value NIL = Node value NIL NIL
insert value (Node v left right)
  | value < v = Node v (insert value left) right
  | value > v = Node v left (insert value right)
  | otherwise = Node v left (Node value NIL right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search value NIL = NIL
search value (Node v left right)
  | value < v = search value left
  | value > v = search value right
  | otherwise = (Node v left right)


--retorna o elmento maximo da BST
myMaximum NIL = Nothing
myMaximum (Node v _ NIL) = Just v
myMaximum (Node v _ right) = myMaximum right

--retorna o elemento minimo da BST
myMinimum NIL = Nothing
myMinimum (Node v NIL _) = Just v
myMinimum (Node v left _) = myMinimum left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor = undefined

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove value NIL = NIL

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node value left right) = [value] ++ preOrder left ++ preOrder right

order NIL = []
order (Node value left right) = order left ++ [value] ++ order right

postOrder NIL = []
postOrder (Node value left right) = postOrder left ++ postOrder right ++ [value]