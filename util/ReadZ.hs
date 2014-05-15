-- a Monadic wrapper around ReadS values, because a non-fully-applied type
-- synonym can't be a Monad itself

module Parsing.ReadZ (
  -- * Type
  ReadZ(..), readz, returns,
  -- * "Text.ParserCombinators.ReadP" compatibility functions
  -- |Not implemented: @gather@
  get, look, (+++), (<++), pfail, eof, satisfy, char, string, munch, munch1,
  skipSpaces, choice, many, many1, sepBy, sepBy1, count, option, optional,
  between, skipMany, skipMany1, endBy, endBy1, manyTill, chainr, chainl,
  chainl1, chainr1,
  -- * New functions
  readToken, readValue, most, most1, skipHoriz, skipHoriz1, skipSpaces1,
  readEntry, readCSV, option', optional', cint, hsint,
 ) where
 import Control.Monad (MonadPlus(..), msum, guard, liftM2)
 import Data.Char (isSpace)
 import Data.List (stripPrefix)
 import Numeric (readHex, readOct, readDec)
 import MoreStrings (horizSpace)

 -- type ReadS a = String -> [(a, String)]
 newtype ReadZ a = ReadZ {runReadZ :: ReadS a}

 instance Functor ReadZ where
  fmap f (ReadZ x) = ReadZ $ \str -> [(f a, b) | (a,b) <- x str]

 instance Monad ReadZ where
  ReadZ x >>= f = ReadZ $ concatMap (uncurry $ runReadZ . f) . x
  return x      = ReadZ $ \s -> [(x,s)]
  fail _        = ReadZ $ const []

 instance MonadPlus ReadZ where
  mzero = ReadZ $ const []
  mplus (ReadZ x) (ReadZ y) = ReadZ $ \str -> x str ++ y str

 readz :: Read a => ReadZ a
 readz = ReadZ reads

 returns :: [a] -> ReadZ a
 returns xs = ReadZ $ \str -> [(x, str) | x <- xs]

 infixr 5 +++, <++

 (+++) :: ReadZ a -> ReadZ a -> ReadZ a  -- cf. Parsing.ReadS.|~|
 (+++) = mplus

 (<++) :: ReadZ a -> ReadZ a -> ReadZ a  -- cf. Parsing.ReadS.<~|
 ReadZ x <++ ReadZ y = ReadZ $ \str -> case x str of [] -> y str; xs -> xs

 readToken :: String -> ReadZ ()
 readToken tok = ReadZ lex >>= guard . (== tok)

 readValue :: Eq a => a -> ReadZ a -> ReadZ ()
 readValue a x = x >>= guard . (== a)

 many :: ReadZ a -> ReadZ [a]
 many x = return [] +++ many1 x

 many1 :: ReadZ a -> ReadZ [a]
 many1 x = liftM2 (:) x (many x)

 skipMany :: ReadZ a -> ReadZ ()
 skipMany p = many p >> return ()

 skipMany1 :: ReadZ a -> ReadZ ()
 skipMany1 p = many1 p >> return ()

 most :: ReadZ a -> ReadZ [a]
 most x = most1 x <++ return []

 most1 :: ReadZ a -> ReadZ [a]
 most1 x = liftM2 (:) x (most x)

 skipHoriz :: ReadZ ()  -- skip horizontal whitespace
 skipHoriz = munch horizSpace >> return ()

 skipHoriz1 :: ReadZ ()  -- skip horizontal whitespace
 skipHoriz1 = munch1 horizSpace >> return ()

 skipSpaces :: ReadZ ()
 skipSpaces = munch isSpace >> return ()

 skipSpaces1 :: ReadZ ()
 skipSpaces1 = munch1 isSpace >> return ()

 string :: String -> ReadZ String
 string msg = ReadZ $ \str -> [(msg, r) | Just r <- [stripPrefix msg str]]

 char :: Char -> ReadZ Char
 char = satisfy . (==)

 pfail :: ReadZ a
 pfail = mzero

 munch :: (Char -> Bool) -> ReadZ String
 munch f = ReadZ $ (: []) . span f

 munch1 :: (Char -> Bool) -> ReadZ String
 munch1 f = ReadZ $ \str -> [(r1, r2) | (r1@(_:_), r2) <- [span f str]]

 choice :: [ReadZ a] -> ReadZ a
 choice = msum

 readEntry :: Eq a => [(a,b)] -> ReadZ a -> ReadZ b
 readEntry dict f = do a <- f; Just b <- return $ lookup a dict; return b

 readCSV :: ReadZ a -> ReadZ [a]
 readCSV f = sepBy f (readToken ",")

 option :: a -> ReadZ a -> ReadZ a
 option x p = p +++ return x

 option' :: ReadZ a -> ReadZ Bool
 option' p = (p >> return True) +++ return False

 optional :: ReadZ a -> ReadZ ()
 optional p = (p >> return ()) +++ return ()

 optional' :: ReadZ a -> ReadZ (Maybe a)
 optional' f = fmap Just f +++ return Nothing

 sepBy :: ReadZ a -> ReadZ b -> ReadZ [a]
 sepBy f del = return [] +++ sepBy1 f del

 sepBy1 :: ReadZ a -> ReadZ b -> ReadZ [a]
 sepBy1 f del = liftM2 (:) f (many $ del >> f)

 get :: ReadZ Char
 get = ReadZ $ \str -> case str of s:tr -> [(s, tr)]; _ -> []

 look :: ReadZ String
 look = ReadZ $ \str -> [(str, str)]

 eof :: ReadZ ()
 eof = look >>= guard . null

 cint :: ReadZ Int  -- Signed ints must be handled with readSigned.
 cint = ReadZ $ \str -> case str of '0':'x':xs -> readHex xs
				    '0':'X':xs -> readHex xs
				    '0':xs     -> readOct xs
				    xs         -> readDec xs

 hsint :: ReadZ Int  -- Signed ints must be handled with readSigned.
 hsint = ReadZ $ \str -> case str of '0':'x':xs -> readHex xs
				     '0':'X':xs -> readHex xs
				     '0':'o':xs -> readOct xs
				     '0':'O':xs -> readOct xs
				     xs         -> readDec xs

 satisfy :: (Char -> Bool) -> ReadZ Char
 satisfy p = do c <- get; guard (p c); return c

 count :: Int -> ReadZ a -> ReadZ [a]
 count n rz = sequence $ replicate n rz

 between :: ReadZ open -> ReadZ close -> ReadZ a -> ReadZ a
 between open close mid = do _ <- open; x <- mid; _ <- close; return x

 endBy :: ReadZ a -> ReadZ sep -> ReadZ [a]
 endBy p sep = many $ do x <- p; _ <- sep; return x

 endBy1 :: ReadZ a -> ReadZ sep -> ReadZ [a]
 endBy1 p sep = many1 $ do x <- p; _ <- sep; return x

 manyTill :: ReadZ a -> ReadZ end -> ReadZ [a]
 manyTill p end = (end >> return []) <++ liftM2 (:) p (manyTill p end)

 chainr :: ReadZ a -> ReadZ (a -> a -> a) -> a -> ReadZ a
 chainr p op x = chainr1 p op +++ return x

 chainl :: ReadZ a -> ReadZ (a -> a -> a) -> a -> ReadZ a
 chainl p op x = chainl1 p op +++ return x

 chainr1 :: ReadZ a -> ReadZ (a -> a -> a) -> ReadZ a
 chainr1 p op = p >>= rest
  where rest x = (do f <- op; y <- chainr1 p op; return $ f x y) +++ return x

 chainl1 :: ReadZ a -> ReadZ (a -> a -> a) -> ReadZ a
 chainl1 p op = p >>= rest
  where rest x = (do f <- op; y <- p; rest $ f x y) +++ return x
