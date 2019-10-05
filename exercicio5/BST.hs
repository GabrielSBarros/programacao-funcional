
module BST (BinaryTree(..),
            insert, order, isBST) where
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
