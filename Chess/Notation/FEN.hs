-- Importing & exporting boards in Forsyth-Edwards Notation

module Chess.Notation.FEN where
 import Control.Monad (guard)
 import Data.Array
 import Data.Char (isDigit, digitToInt)
 import Data.List (intercalate, groupBy)
 import Data.Maybe (isJust)
 import Numeric (readDec)
 import Chess
 import Chess.Notation
 import Parsing.ReadZ
 import Ternary

 showsFEN :: (Board, Int, Int) -> ShowS
 showsFEN (game, halves, full) = showString (intercalate "/" $ map (\r ->
   do t <- groupBy (\a b -> a == b && a == Nothing)
		   [getTile game (f,r) | f <- [FileA .. FileH]]
      case t of [Just p] -> showsPiece' p ""
		_        -> show $ length t) [Rank8, Rank7 .. Rank1])
  . (' ' :) . (b_player game == White ?: ('w' :) :? ('b' :))
  . (' ' :) . (null castles ?: ('-' :) :? showString castles)
  . (' ' :)
  . (maybe ('-' :)
	   (\f -> showsFile f . (b_player game == White ?: ('6' :) :? ('3' :)))
	   $ b_passant game)
  . (' ' :) . shows halves
  . (' ' :) . shows full
  where castles = (b_canCastleWK game ?: ('K' :) :? id)
		$ (b_canCastleWQ game ?: ('Q' :) :? id)
		$ (b_canCastleBK game ?: ('k' :) :? id)
		$ (b_canCastleBQ game ?: "q" :? "")

 readsFEN :: ReadS (Board, Int, Int)
 readsFEN = runReadZ $ do skipSpaces
			  rows <- sequence $ row : replicate 7 (char '/' >> row)
			  skipSpaces1
			  side <- (char 'w' >> return White)
			      +++ (char 'b' >> return Black)
			  skipSpaces1
			  castles <- munch1 (`elem` "KQkq") <++ string "-"
			  skipSpaces1
			  passant <- (do (f,r) <- ReadZ readsTile
					 guard  $ side == White ?: r == Rank6
								:? r == Rank3
					 return $ Just f)
				  +++ (string "-" >> return Nothing)
			  skipSpaces1
			  halves <- ReadZ readDec
			  skipSpaces1
			  full <- ReadZ readDec
			  return (Board {
			    b_player = side,
			    b_board = array (minBound, maxBound) $ concat
			     $ zipWith (\r -> map $ \(f,p) -> ((f,r), p))
				       [Rank8, Rank7 .. Rank1] rows,
			    b_canCastleWK = elem 'K' castles,
			    b_canCastleWQ = elem 'Q' castles,
			    b_canCastleBK = elem 'k' castles,
			    b_canCastleBQ = elem 'q' castles,
			    b_passant = passant
			   }, halves, full)
  where row = fmap (zip [FileA .. FileH] . concat) $ most1
	       $ fmap ((: []) . Just) (ReadZ readsPiece') +++ blanks
	blanks = do x <- fmap digitToInt $ satisfy isDigit
		    guard  $ 1 <= x && x <= 8
		    return $ replicate x Nothing

 finalFEN :: [Move] -> (Board, Int, Int)
 finalFEN = foldl (\(_, half, full) m -> (m_after m,
		    m_piece m == Pawn || isJust (m_captured m) ?: 0 :? half+1,
		    m_player m == Black ?: full+1 :? full)) (start, 0, 1)
