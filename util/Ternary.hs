module Ternary where
 data TernaryBranch a = a :? a deriving (Eq, Ord, Read, Show, Bounded)

 instance Functor TernaryBranch where fmap f (x :? y) = f x :? f y

 infixr 0 ?:, :?, ?~

 (?:) :: Bool -> TernaryBranch a -> a
 True  ?: (y :? _) = y
 False ?: (_ :? z) = z

 (?~) :: Bool -> (a, a) -> a
 True  ?~ (y, _) = y
 False ?~ (_, z) = z
