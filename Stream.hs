import Data.Maybe (fromJust, fromMaybe)
-- For testing only
import qualified Data.Map as Map


data Stream a = Empty | Cons a (Stream a) deriving (Show, Eq)

foldRight :: (a -> b -> b) -> b -> Stream a -> b
foldRight _ z Empty = z
foldRight f z (Cons x xs) = x `f` z' where z' = (foldRight f z xs)

foldLeft :: (b -> a -> b) -> b -> Stream a -> b
foldLeft _ z Empty = z
foldLeft f z (Cons x xs) = seq z' (foldLeft f z') xs where z' = z `f` x

tailS :: Stream a -> Stream a
tailS Empty = Empty
tailS (Cons _ xs) = xs

-- (1) more specific than fold: it returns stream
-- (2) dual/opposite of fold/recursion: go from base case to recursive case
-- (3) function is element to element (with state)
unfold :: (a -> Maybe (a, b)) -> a -> Stream b
unfold f z = case f z of
  Just (z', x) -> Cons x (unfold f z')
  _ -> Empty

scanRight :: (a -> b -> b) -> b -> Stream a -> Stream b
-- scanRight _ z Empty = Cons z Empty
-- scanRight f z (Cons x xs) = Cons (x `f` head z') z'
--   where z' = (scanRight f z xs)
scanRight f z = foldRight g (Cons z Empty)
  where g x z = Cons (x `f` headS z) z

--
-- Built by core functions
--

-- headOption :: Stream a -> Maybe a
-- headOption Empty = Nothing
-- headOption (Cons a _) = Just a
headOption :: Stream a -> Maybe a
headOption = foldRight (\x _ -> Just x) Nothing

headS :: Stream a -> a
headS = fromJust . headOption

apply :: [a] -> Stream a
apply = foldr Cons Empty

toList :: Stream a -> [a]
toList = foldRight (:) []

-- takeWhile can be seen as extension of foldRight with a predicate
-- where the second operand z' can have two forms depending on the predicate
-- evaluated on the head x
-- takeWhileS :: (a -> Bool) -> Stream a -> Stream a
-- takeWhileS _ Empty = Empty
-- takeWhileS p (Cons x xs) | p x = Cons x (takeWhileS p xs)
--                          | otherwise = Empty
takeWhileS :: (a -> Bool) -> Stream a -> Stream a
takeWhileS p = foldRight f Empty
  where f x z | p x = Cons x z
              | otherwise = Empty

-- take is probably harder to implement in foldRight, if not impossible
-- The problem is that the predicate has a state: the i in n - i
-- foldRight however requires the function to be stateless
takeS :: Int -> Stream a -> Stream a
-- takeS _ Empty = Empty
-- takeS n (Cons x xs) | n > 0 = Cons x (takeS (n - 1) xs)
--                     | otherwise = Empty
takeS n xs = unfold f (n, xs)
  where f (_, Empty) = Nothing
        f (n, Cons x xs) | n > 0 = Just ((n - 1, xs), x)
                    | otherwise = Nothing


-- It's important that predicate appllies to head x first in order to get
-- the short-circuit effect
exists :: (a -> Bool) -> Stream a -> Bool
exists p = foldRight (\x z -> p x || z) False

-- How to factor out the && and || betwen forAll and exists?  Monoid?
forAll :: (a -> Bool) -> Stream a -> Bool
forAll p = foldRight (\x z -> p x && z) True

mapS :: (a -> b) -> Stream a -> Stream b
mapS f = foldRight (\x z -> Cons (f x) z) Empty

mapS2 f = unfold g
  where g Empty = Nothing
        g (Cons x xs) = Just (xs, (f x))

filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p = foldRight f Empty
  where f x z | p x = Cons x z
              | otherwise = z

appendS :: a -> Stream a -> Stream a
appendS x = foldRight Cons (Cons x Empty)

concatS :: Stream a -> Stream a -> Stream a
concatS xs ys = foldRight Cons ys xs

flatMap :: (a -> Stream b) -> Stream a -> Stream b
flatMap f = foldRight (\x z -> f x `concatS` z) Empty

find :: Eq a => a -> Stream a -> Maybe a
find x = headOption . filterS (x==)

-- Similar to take, cannot be expressed by fold, because operator has state that
-- changes from element to element
zipS :: Stream a -> Stream b -> Stream (a, b)
-- zipS Empty _ = Empty
-- zipS _ Empty = Empty
-- zipS (Cons x xs) (Cons y ys) = Cons (x, y) (zipS xs ys)
zipS xs ys = unfold f (xs, ys)
  where f (Empty, _) = Nothing
        f (_, Empty) = Nothing
        f (Cons x xs, Cons y ys) = Just ((xs, ys), (x, y))

-- zipAll :: Stream a -> Stream b -> Stream (Maybe a, Maybe b)
-- zipAll xs ys = unfold f (xs, ys)
--   where f (Empty, Empty) = Nothing
--         f (x:xs, Empty) = Just ((xs, Empty), (Just x, Nothing))
--         f (Empty, y:ys) = Just ((Empty, ys), (Nothing, Just y))
--         f (x:xs, y:ys) = Just ((xs, ys), (Just x, Just y))

ones :: Stream Int
-- ones = Cons 1 ones
ones = constant 1

constant :: a -> Stream a
-- constant x = Cons x (constant x)
constant = unfold (\z -> Just (z, z))

from :: Int -> Stream Int
-- from n = Cons n $ from (n + 1)
from = unfold (\z -> Just (z + 1, z))

fibs :: Stream Int
fibs = Cons 0 $ Cons 1 $ mapS (\(x,y) -> x + y) . zipS fibs $ tailS fibs

fibs2 :: Stream Int
fibs2 = Cons 0 $ Cons 1 $ f 0 1
  where f x y = let z = x + y in Cons z (f y z) 

fibs3 :: Stream Int
fibs3 = Cons 0 $ Cons 1 $ unfold f (0, 1)
  where f (x, y) = let z = x + y in Just ((y, z), z)

unfold2 :: (a -> Maybe (a, b)) -> a -> Stream b
unfold2 f z = foldr g Empty (f z)
  where g (z, x) _ = Cons x (unfold2 f z)

unfold3 :: (a -> Maybe (a, b)) -> a -> Stream b
unfold3 f z = -- fromMaybe Empty $ fmap g (f z)
  maybe Empty g (f z)
  where g (z, x) = Cons x (unfold3 f z)


testAll :: IO ()
testAll =
  let s = Cons 1 $ Cons 2 $ Cons 3 Empty
      l = [1, 2, 3]
      e = Empty :: Stream Int
      testCases = Map.fromList [
          ("apply", apply [1, 2, 3] == s),
          ("headOption 1", headOption s == Just 1),
          ("headOption 2", headOption e == Nothing),
          ("foldRight 1", foldRight (:) [] s == l),
          ("foldRight 2", foldRight (:) [] e == []),
          ("foldLeft 1", foldLeft (+) 0 e == 0),
          ("foldLefet 2", foldLeft (\z x -> x:z) [] s == reverse l),
          ("toList 1", toList e == []),
          ("toList 2", toList s == l),
          ("takeS a", takeS 0 s == e),
          ("takeS b", takeS 1 e == e),
          ("takeS c", takeS 2 s == (Cons 1 $ Cons 2 Empty)),
          ("takeS d", takeS 10 s == s),
          ("takeWhileS a", takeWhileS (<3) s == (Cons 1 $ Cons 2 Empty)),
          ("exists a", exists (==2) s == True)
          ]
      failedCases = Map.filter not testCases
  in putStrLn $ "Failed cases: " ++ show (Map.keys failedCases)
