import qualified Prelude as P

data List a = Cons a (List a) | Nil deriving P.Show

prepend :: a -> List a -> List a
prepend x xs = (Cons x xs)

(+) :: a -> List a -> List a
(+) x xs = x `prepend` xs


(++) :: List a -> List a -> List a
(++) (Cons x Nil) ys = (Cons x ys) 
(++) (Cons x xs) ys = x + (xs ++ ys)


fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = (Cons x (fromList xs))


reducel :: (a -> b -> a) -> a -> List b -> a
reducel f x Nil = x 
reducel f x (Cons y ys) = reducel f (f x y) ys 



length :: List a -> P.Integer
length Nil = 0
length (Cons x xs) = 1 P.+ length xs

find :: P.Eq a => List a -> a -> P.Maybe a
find Nil a = P.Nothing
find (Cons x xs) a = if (x P.== a) then P.Just x else find xs a

