{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- cf. <http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm>

module Chess.Notation.PGN (
  -- * Types
  PGNGame,
  -- ** Movetext section items
  PGNElem(..), moves2pgn, pgn2moves,
  -- ** Game termination markers
  Termination(..), readsTermination, showsTermination,
  -- * Reading & showing PGN data
  readsPGN, readsPGNGame, readsPGNGames,
  showsPGN, showsPGNGame, showsPGNGames,
 ) where
 import Control.Monad (liftM2, liftM3, guard)
 import Data.Char (isAlphaNum)
 import Data.Ix (Ix)
 import Data.Maybe (fromJust)
 import Numeric (readDec)
 import Text.ParserCombinators.ReadP
 import Chess
 import Chess.Notation
 import Chess.Util

 type PGNGame = ([(String, String)], [PGNElem], Termination)

 data PGNElem = PGNMove SANMove | NAG Int | RAV [PGNElem]
  deriving (Eq, Ord, Read, Show)

 pgn2moves :: Board -> [PGNElem] -> [[Move]]
 pgn2moves b (PGNMove sm:xs) = do move <- san2move b sm
				  rest <- pgn2moves (m_after move) xs
				  return $ move : rest
 pgn2moves b (_:xs) = pgn2moves b xs
 pgn2moves _ [] = return []

 moves2pgn :: [Move] -> [PGNElem]
 moves2pgn = map (PGNMove . move2san)

 data Termination = WhiteWins | BlackWins | DrawnGame | Other
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 showsTermination :: Termination -> ShowS
 showsTermination WhiteWins = ("1-0" ++)
 showsTermination BlackWins = ("0-1" ++)
 showsTermination DrawnGame = ("1/2-1/2" ++)
 showsTermination Other = ('*' :)

 readsTermination :: ReadS Termination
 -- TODO: Should this skip leading whitespace?
 readsTermination ('1':'-':'0':xs) = [(WhiteWins, xs)]
 readsTermination ('0':'-':'1':xs) = [(BlackWins, xs)]
 readsTermination ('1':'/':'2':'-':'1':'/':'2':xs) = [(DrawnGame, xs)]
 readsTermination ('*':xs) = [(Other, xs)]
 readsTermination _ = []

 showsPGN :: SANMove -> ShowS  -- show a move in PGN algebraic notation
 showsPGN sm = (if sm_shortCastle sm then showString "O-O"
		else if sm_longCastle sm then showString "O-O-O"
		else showsPiece (sm_piece sm)
		      . (sm_piece sm == Pawn
			 ?: (sm_capture sm
			     ?: maybe id showsFile (sm_fromFile sm) . ('x' :)
			     :? id)
			 :? maybe id showsFile (sm_fromFile sm)
			  . maybe id showsRank (sm_fromRank sm)
			  . (sm_capture sm ?: ('x' :) :? id))
		      . showsSquare (fromJust $ sm_to sm)
		      . maybe id (\p -> ('=' :) . showsPiece p)
				  (sm_promotion sm))
		. (sm_checkmates sm ?: ('#' :) :? sm_checks sm ?: ('+' :) :? id)

 readsPGN :: ReadS SANMove
 readsPGN = readsSAN

 showsPGNGame :: PGNGame -> ShowS
 -- Should this allow for move sequences that begin with Black's move and/or at
 -- a move number other than 1?
 showsPGNGame (tags, moves, end) = foldr (.) id [
   ('[' :) . showString n . (' ' :) . shows tval . (']' :) . ('\n' :)
   | (tname, tval) <- tags,
     let n = dropWhile (== '_') $ map (\c -> isAlphaNum c ?: c :? '_') tname,
     not $ null n
  ] . ('\n' :)
    . showString (unlines $ wrapLine 79 $ tail $ shmoves 1 True moves
						 ++ ' ':showsTermination end "")
    . ('\n' :)
  where shmoves i isW (PGNMove m:xs) = pre ++ ' ' : showsPGN m ""
					   ++ shmoves j (not isW) xs
	 where (pre, j) = i == 0 ?: ("", i :: Int)
				 :? isW ?: (' ' : show i ++ ".", i) :? ("", i+1)
	shmoves i isW (NAG x:xs) = ' ':'$':show x ++ shmoves i isW xs
	shmoves i isW (RAV x:xs) = '(':shmoves 0 isW x ++ ')':shmoves i isW xs
	shmoves _ _ [] = ""

 readsPGNGame :: ReadS PGNGame
 -- Should this care about the move numbers at all?
 readsPGNGame = readP_to_S $ liftM3 (,,) (most tag) (most relem)
					 (readS_to_P readsTermination)
  where symchr c = isAlphaNum c || c `elem` "_+#=:-"
	symbol = liftM2 (:) (satisfy isAlphaNum) (munch symchr)
	tag = do pgnSP
		 char '['; pgnSP
		 tname  <- symbol; pgnSP
		 tvalue <- readS_to_P reads; pgnSP
		 char ']'
		 return (tname, tvalue)
	relem = do pgnSP
		   most $ int >> pgnSP >> munch (== '.') >> pgnSP
		   el <- between (char '(') (char ')') (fmap RAV $ most relem)
			  +++ (char '$' >> fmap NAG (readS_to_P readDec))
			  +++ (do sym <- symbol
				  choice [return $ PGNMove m
					  | (m, "") <- readsSAN sym])
	           pgnSP
		   most $ int >> pgnSP >> munch (== '.') >> pgnSP
		   return el
	int = do readS_to_P readDec :: ReadP Int
		 ahead <- look
		 guard $ null ahead || not (symchr $ head ahead)
		 -- The above prevents moves beginning with digits (rare and
		 -- arguably invalid though they may be) from being ambiguously
		 -- parsed as move numbers followed by a move.

 showsPGNGames :: [PGNGame] -> ShowS
 showsPGNGames = foldr (.) id . map showsPGNGame

 readsPGNGames :: ReadS [PGNGame]
 readsPGNGames = readP_to_S $ most $ do g <- readS_to_P readsPGNGame
					pgnSP
					return g

 pgnSP :: ReadP ()  -- not for export
 pgnSP = (>> return ()) $ most $ skipSpaces1
	  +++ (char ';' >> munch (/= '\n') >> (char '\n' >> return ()) +++ eof)
	  +++ (char '{' >> munch (/= '}') >> char '}' >> return ())
