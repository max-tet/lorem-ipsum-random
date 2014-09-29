module WList
( WList (Empty, Leaf, Node)
, distribFromList
, pickRandom
) where

import qualified System.Random as Random

data WList a = Empty
	| Leaf {w :: Int, e :: a}
	| Node {l :: WList a, r :: WList a }

instance (Show a) => Show (WList a) where
	show = showPretty ""
	
showPretty :: Show a => String -> WList a -> String
showPretty pre nd@(Node l r) = pre ++"Node (" ++ show (weight nd) ++ ")\n" ++
	pre ++ "|\\\n" ++
	(showPretty (pre++"| ") r) ++ "\n" ++
	pre ++ " \\\n" ++ 
	(showPretty (pre++"  ") l)
showPretty pre (Leaf w e) = pre ++ "Leaf (" ++ show w ++ ") " ++ show e
showPretty pre Empty = pre ++ "Empty\n"

weight :: WList a -> Int
weight Empty = 0
weight (Leaf w _) = w
weight (Node l r) = (weight l) + (weight r)

fromTupleList :: [(Int,a)] -> WList a
fromTupleList [] = Empty
fromTupleList (x:xs) = insert x (fromTupleList xs)

distribFromList :: Eq b => (a -> b) -> [a] -> WList.WList b
distribFromList f [] = WList.Empty
distribFromList f (x:xs) = WList.bumpOrInsert (f x) (distribFromList f xs)

insert :: (Int, a) -> WList a -> WList a
insert (w, e) Empty = Leaf w e
insert (w, e) (Leaf lw le) = Node (Leaf lw le) (Leaf w e)
insert (w, e) (Node node_left node_right) = 
	if weight node_left < weight node_right 
	then Node (insert (w,e) node_left) node_right
	else Node node_left (insert (w,e) node_right)

bump :: Eq a => a -> WList a -> (WList a, Bool)
bump x Empty = (Empty, False)
bump x l@(Leaf w e) = if x == e
	then (Leaf (w+1) e, True)
	else (l, False)
bump x (Node l r) = if snd bl then (Node (fst bl) r, True) else (Node l (fst br), snd br)
	where bl = bump x l
	      br = bump x r

bumpOrInsert :: Eq a => a -> WList a -> WList a
bumpOrInsert x wl = if snd bwl then fst bwl else insert (1, x) wl
	where bwl = bump x wl

pickByWeight :: Int -> WList a -> Maybe a
pickByWeight _ Empty = Nothing
pickByWeight _ (Leaf _ e) = Just e
pickByWeight w (Node l r) = if w <= weight l then pickByWeight w l else pickByWeight (w - weight l) r

pickRandom :: Random.RandomGen g => WList a -> g -> (Maybe a, g)
pickRandom wl gen = (pickByWeight rnd wl, nextgen)
	where (rnd, nextgen) = Random.randomR (1, weight wl) gen