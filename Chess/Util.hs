{-# OPTIONS_HADDOCK hide #-}

-- |Internal helper functions

module Chess.Util where
 import Control.Monad (liftM2)
 import Data.Char (isSpace)
 import Text.ParserCombinators.ReadP

 data TernaryBranch a = a :? a deriving (Eq, Ord, Read, Show, Bounded)

 instance Functor TernaryBranch where fmap f (x :? y) = f x :? f y

 infixr 0 ?:, :?

 (?:) :: Bool -> TernaryBranch a -> a
 True  ?: (y :? _) = y
 False ?: (_ :? z) = z

 wrapLine :: Int -> String -> [String]
 wrapLine _ [] = []
 wrapLine len str = first : wrapLine len rest
  where (first, rest) = wrap' 0 0 str
	wrap' _ _ [] = (str, [])
	wrap' lst trail s = if lst > 0 && lst + trail + length a > len
			    then (take lst str, drop (lst + trail) str)
			    else wrap' (lst+trail+length a) (length ws) b'
	 where (a, b) = span (not . breakable) s
	       (ws, b') = span breakable b
	breakable c = isSpace c && c /= '\xA0'

 option' :: ReadP a -> ReadP Bool
 option' p = (p >> return True) +++ return False

 optional' :: ReadP a -> ReadP (Maybe a)
 optional' f = fmap Just f +++ return Nothing

 skipSpaces1 :: ReadP ()
 skipSpaces1 = munch1 isSpace >> return ()

 -- |@most@ and @most1@ parse as many instances of the given parser as
 -- possible, without creating any \"branches\" for stopping early.
 most, most1 :: ReadP a -> ReadP [a]
 most  p = most1 p <++ return []
 most1 p = liftM2 (:) p (most p)
