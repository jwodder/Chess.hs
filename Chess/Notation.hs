-- |Functions for working with standard algebraic notation in accordance with
-- the FIDE Laws of Chess
-- <http://www.fide.com/component/handbook/?id=124&view=article>.

-- All of the functions that read & show chess pieces use English-based
-- abbreviations: @K@ for 'King', @Q@ for 'Queen', @B@ for 'Bishop', @N@ for
-- 'Knight', @R@ for 'Rook', and @P@ (when used) for 'Pawn'.

module Chess.Notation (
  -- * Reading basic chess data
  readsPiece, readsPiece', readsSquare, readsFile, readsRank,
  -- * Showing basic chess data
  showsPiece, showsPiece', showsSquare, showsFile, showsRank,
  -- * The 'SANMove' type
  SANMove(..), move2san, move2san', san2move,
  -- * Reading & showing moves
  readsSAN, readsMove,
  showsSAN, showsMove,
 ) where
 import Control.Monad (guard, liftM2)
 import Data.Array
 import Data.Char (toLower, intToDigit, isSpace)
 import Data.Maybe (isJust, maybeToList, fromJust)
 import Text.ParserCombinators.ReadP
 import Chess
 import Chess.Util

 -- |A representation of the possible information that can be encoded in a
 -- standard algebraic notation move
 data SANMove = SANMove {
  sm_shortCastle :: Bool,
  sm_longCastle  :: Bool,
  sm_piece       :: Piece,
  sm_fromFile    :: Maybe File,
  sm_fromRank    :: Maybe Rank,
  sm_capture     :: Bool,
  sm_to          :: Maybe Square,  -- 'Nothing' iff the move is a castle
  sm_passant     :: Bool,
  sm_promotion   :: Maybe Piece,
  sm_checks      :: Bool,
  sm_checkmates  :: Bool
 } deriving (Eq, Ord, Read, Show)

 -- |'ShowS' for converting a 'SANMove' to standard algebraic notation
 showsSAN :: SANMove -> ShowS
 showsSAN sm = if sm_shortCastle sm then showString "0-0"
	       else if sm_longCastle sm then showString "0-0-0"
	       else showsPiece (sm_piece sm)
		     . (sm_piece sm == Pawn
		        ?: (sm_capture sm
			    ?: maybe id showsFile (sm_fromFile sm) . ('x' :)
			    :? id)
			:? maybe id showsFile (sm_fromFile sm)
			 . maybe id showsRank (sm_fromRank sm)
			 . (sm_capture sm ?: ('x' :) :? id))
		     . showsSquare (fromJust $ sm_to sm)
		     . (sm_passant sm ?: showString "e.p." :? id)
		     . maybe id showsPiece (sm_promotion sm)

 readsSAN :: ReadS SANMove  -- read a SAN or PGN move
 readsSAN = readP_to_S $ do
  -- TODO: Should this support check(mate) indicators before _en passant_
  -- indicators?
  skipSpaces
  sm <- read00 +++ read000 +++ readNorm
  (skipSpaces >> char '+' >> return (sm {sm_checks = True}))
   +++ (skipSpaces >> (string "#" +++ string "++") >> return (sm {sm_checkmates = True}))
   +++ return sm
  where read00 = string "0-0" +++ string "O-O" >> return (SANMove True False King Nothing Nothing False Nothing False Nothing False False)
	read000 = string "0-0-0" +++ string "O-O-O" >> return (SANMove False True King Nothing Nothing False Nothing False Nothing False False)
	readNorm = do piece <- readS_to_P readsPiece
		      f1 <- optional' $ readS_to_P readsFile
		      r1 <- optional' $ readS_to_P readsRank
		      capt <- option' $ char 'x'
		      to <- readS_to_P readsSquare
		      ep <- option' $ skipSpaces >> string "e.p." >> skipSpaces
		      promo <- optional' $ optional (char '=')
					    >> readS_to_P readsPiece
		      guard $ promo /= Just Pawn && promo /= Just King
		      return $ SANMove False False piece f1 r1 capt (Just to)
				       ep promo False False

 -- |'ShowS' for a 'Piece' in English-based algebraic notation.  Pieces are
 -- represented as uppercase letters, except 'Pawn', which is represented by an
 -- empty string.
 showsPiece :: Piece -> ShowS
 showsPiece King   = ('K' :)
 showsPiece Queen  = ('Q' :)
 showsPiece Bishop = ('B' :)
 showsPiece Knight = ('N' :)
 showsPiece Rook   = ('R' :)
 showsPiece Pawn   = id

 -- |'ShowS' for a 'Piece' with 'Player' information, with white's pieces being
 -- written in uppercase and black's in lowercase.  'Pawn's are explicitly
 -- written as @P@ or @p@.
 showsPiece' :: (Piece, Player) -> ShowS
 showsPiece' (p, side) = ((side == White ?: id :? toLower) (short ! p) :)
  where short = listArray (Pawn, King) "PRNBQK"

 -- |'ReadS' for a 'Piece' in English-based algebraic notation.  'Pawn' is
 -- represented by an empty string and is returned if & only if none of the
 -- other options matched.  Matches must be in uppercase.  Leading whitespace
 -- is skipped.
 readsPiece :: ReadS Piece
 readsPiece str = case dropWhile isSpace str of
		   'K':xs -> [(King,   xs)]
		   'Q':xs -> [(Queen,  xs)]
		   'B':xs -> [(Bishop, xs)]
		   'N':xs -> [(Knight, xs)]
		   'R':xs -> [(Rook,   xs)]
		   xs     -> [(Pawn,   xs)]

 -- |'ReadS' for a 'Piece' with 'Player' information in which white's pieces
 -- are written in uppercase and black's in lowercase.  'Pawn's must be
 -- explicitly written as @P@ or @p@.  Leading whitespace is skipped.
 readsPiece' :: ReadS (Piece, Player)
 readsPiece' str = case dropWhile isSpace str of
		    'K':xs -> [((King,   White), xs)]
		    'Q':xs -> [((Queen,  White), xs)]
		    'B':xs -> [((Bishop, White), xs)]
		    'N':xs -> [((Knight, White), xs)]
		    'R':xs -> [((Rook,   White), xs)]
		    'P':xs -> [((Pawn,   White), xs)]
		    'k':xs -> [((King,   Black), xs)]
		    'q':xs -> [((Queen,  Black), xs)]
		    'b':xs -> [((Bishop, Black), xs)]
		    'n':xs -> [((Knight, Black), xs)]
		    'r':xs -> [((Rook,   Black), xs)]
		    'p':xs -> [((Pawn,   Black), xs)]
		    _      -> []

 -- |'ShowS' for a 'Square' represented as a 'File' (as shown by 'showsFile')
 -- followed by a 'Rank' (as shown by 'showsRank').
 showsSquare :: Square -> ShowS
 showsSquare (f,r) = showsFile f . showsRank r

 -- |'ReadS' for a 'Square' represented as a 'File' (as read by 'readsFile')
 -- followed by a 'Rank' (as read by 'readsRank').  Leading and intervening
 -- whitespace is skipped.
 readsSquare :: ReadS Square
 readsSquare txt = do (x, r1) <- readsFile txt
		      (y, r2) <- readsRank r1
		      return ((x,y), r2)

 -- |'ShowS' for a 'File' represented as a lowercase letter from @a@ to @h@
 showsFile :: File -> ShowS
 showsFile f = (toEnum (fromEnum f + 97) :)

 -- |'ReadS' for a 'File' represented as a letter from @a@ to @h@.  Matches are
 -- case-insensitive.  Leading whitespace is skipped.
 readsFile :: ReadS File
 readsFile str = case dropWhile isSpace str of
		  'a':xs -> [(FileA, xs)]
		  'A':xs -> [(FileA, xs)]
		  'b':xs -> [(FileB, xs)]
		  'B':xs -> [(FileB, xs)]
		  'c':xs -> [(FileC, xs)]
		  'C':xs -> [(FileC, xs)]
		  'd':xs -> [(FileD, xs)]
		  'D':xs -> [(FileD, xs)]
		  'e':xs -> [(FileE, xs)]
		  'E':xs -> [(FileE, xs)]
		  'f':xs -> [(FileF, xs)]
		  'F':xs -> [(FileF, xs)]
		  'g':xs -> [(FileG, xs)]
		  'G':xs -> [(FileG, xs)]
		  'h':xs -> [(FileH, xs)]
		  'H':xs -> [(FileH, xs)]
		  _      -> []

 -- |'ShowS' for a 'Rank' represented as a digit from @1@ to @8@
 showsRank :: Rank -> ShowS
 showsRank = (:) . intToDigit . succ . fromEnum

 -- |'ReadS' for a 'Rank' represented as a digit from @1@ to @8@.  Leading
 -- whitespace is skipped.
 readsRank :: ReadS Rank
 readsRank str = case dropWhile isSpace str of
		  '1':xs -> [(Rank1, xs)]
		  '2':xs -> [(Rank2, xs)]
		  '3':xs -> [(Rank3, xs)]
		  '4':xs -> [(Rank4, xs)]
		  '5':xs -> [(Rank5, xs)]
		  '6':xs -> [(Rank6, xs)]
		  '7':xs -> [(Rank7, xs)]
		  '8':xs -> [(Rank8, xs)]
		  _      -> []

 move2san :: Move -> SANMove
 -- fills in sm_from* only as little as necessary to eliminate ambiguity
 move2san m = SANMove {
  sm_shortCastle = m_isShortCastle m,
  sm_longCastle  = m_isLongCastle m,
  sm_piece       = m_piece m,
  sm_fromFile    = frF,
  sm_fromRank    = frR,
  sm_capture     = isJust $ m_captured m,
  sm_to          = Just $ m_to m,
  sm_passant     = m_isEnPassant m,
  sm_promotion   = m_promoted m,
  sm_checks      = m_checks m,
  sm_checkmates  = m_checkmates m
 } where (frF, frR) = if m_piece m == Pawn
			   then (m_captured m >> return f, Nothing)
		      else if length fs == 1
			   then (Nothing, Nothing)
		      else case (filter (== f) fs, filter (== r) rs) of
				([_], _) -> (Just f,  Nothing)
				(_, [_]) -> (Nothing, Just r)
				_        -> (Just f,  Just r)
	 (f,  r)  = m_from m
	 (fs, rs) = unzip $ map m_from $ pieceTo (m_before m) (m_piece m)
						 (m_to m)

 move2san' :: Move -> SANMove  -- fills in sm_from* completely
 move2san' m = SANMove {
  sm_shortCastle = m_isShortCastle m,
  sm_longCastle  = m_isLongCastle m,
  sm_piece       = m_piece m,
  sm_fromFile    = Just $ fst $ m_from m,
  sm_fromRank    = Just $ snd $ m_from m,
  sm_capture     = isJust $ m_captured m,
  sm_to          = Just $ m_to m,
  sm_passant     = m_isEnPassant m,
  sm_promotion   = m_promoted m,
  sm_checks      = m_checks m,
  sm_checkmates  = m_checkmates m
 }

 san2move :: Board -> SANMove -> [Move]
 san2move game sm = case (sm_shortCastle sm, sm_longCastle sm, sm_to sm) of
  (True,  False, _)       -> maybeToList $ shortCastle game
  (False, True,  _)       -> maybeToList $ longCastle  game
  (False, False, Just to) -> do
   move <- pieceTo game (sm_piece sm) to
   guard $ maybe True (fst (m_from move) ==) (sm_fromFile sm)
   guard $ maybe True (snd (m_from move) ==) (sm_fromRank sm)
   guard $ liftM2 (==) (sm_promotion sm) (m_promoted move) /= Just False
   return move
   -- TODO: When promotions are given for non-promotional moves, should the
   -- conversion fail?
   -- TODO: When promotions are omitted for promotional moves, should only
   -- promotion to Queen be returned?
  _ -> []

 showsMove :: Move -> ShowS
 showsMove = showsSAN . move2san

 readsMove :: Board -> ReadS Move
 readsMove game str = do (san, r1) <- readsSAN str
			 move <- san2move game san
			 return (move, r1)
