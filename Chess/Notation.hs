-- Add support for ICCF numeric notation?
-- Add other notations listed at <http://en.wikipedia.org/wiki/Chess_notation>?

-- Should the "basic" reading functions skip leading whitespace?

module Chess.Notation (
  -- * Reading & showing basic chess data
  readsPiece, readsPiece', readsTile, readsFile, readsRank,
  showsPiece, showsPiece', showsTile, showsFile, showsRank,
  -- * The 'SANMove' type
  SANMove(..), move2san, move2san', san2move,
  -- * Reading & showing moves
  readsSAN, readsMove,
  showsSAN, showsMove,
 ) where
 import Control.Monad (guard, liftM2)
 import Data.Array
 import Data.Char (toLower, intToDigit)
 import Data.Maybe (isJust, maybeToList, fromJust)
 import Chess
 import Parsing.ReadZ
 import Ternary

 data SANMove = SANMove {
  sm_shortCastle :: Bool,
  sm_longCastle  :: Bool,
  sm_piece       :: Piece,
  sm_fromFile    :: Maybe File,
  sm_fromRank    :: Maybe Rank,
  sm_capture     :: Bool,
  sm_to          :: Maybe Tile,  -- 'Nothing' iff the move is a castle
  sm_passant     :: Bool,
  sm_promotion   :: Maybe Piece,
  sm_checks      :: Bool,
  sm_checkmates  :: Bool
 } deriving (Eq, Ord, Read, Show)

 showsSAN :: SANMove -> ShowS  -- show a move in FIDE SAN
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
		     . showsTile (fromJust $ sm_to sm)
		     . (sm_passant sm ?: showString "e.p." :? id)
		     . maybe id showsPiece (sm_promotion sm)

 readsSAN :: ReadS SANMove  -- read a SAN or PGN move
 readsSAN = runReadZ $ do
  -- Should this support check(mate) indicators before _en passant_ indicators?
  skipSpaces
  sm <- read00 +++ read000 +++ readNorm
  (skipSpaces >> char '+' >> return (sm {sm_checks = True}))
   +++ (skipSpaces >> (string "#" +++ string "++") >> return (sm {sm_checkmates = True}))
   +++ return sm
  where read00 = string "0-0" +++ string "O-O" >> return (SANMove True False King Nothing Nothing False Nothing False Nothing False False)
	read000 = string "0-0-0" +++ string "O-O-O" >> return (SANMove False True King Nothing Nothing False Nothing False Nothing False False)
	readNorm = do piece <- ReadZ readsPiece
		      f1 <- optional' $ ReadZ readsFile
		      r1 <- optional' $ ReadZ readsRank
		      capt <- option' $ char 'x'
		      to <- ReadZ readsTile
		      ep <- option' $ skipSpaces >> string "e.p." >> skipSpaces
		      promo <- optional' $ optional (char '=')>>ReadZ readsPiece
		      guard $ promo /= Just Pawn && promo /= Just King
		      return $ SANMove False False piece f1 r1 capt (Just to)
				       ep promo False False

 showsPiece :: Piece -> ShowS
 showsPiece King   = ('K' :)
 showsPiece Queen  = ('Q' :)
 showsPiece Bishop = ('B' :)
 showsPiece Knight = ('N' :)
 showsPiece Rook   = ('R' :)
 showsPiece Pawn   = id

 showsPiece' :: (Piece, Player) -> ShowS
 showsPiece' (p, side) = ((side == White ?: id :? toLower) (short ! p) :)
  where short = listArray (Pawn, King) "PRNBQK"

 readsPiece :: ReadS Piece
 readsPiece ('K':xs) = [(King,   xs)]
 readsPiece ('Q':xs) = [(Queen,  xs)]
 readsPiece ('B':xs) = [(Bishop, xs)]
 readsPiece ('N':xs) = [(Knight, xs)]
 readsPiece ('R':xs) = [(Rook,   xs)]
 readsPiece xs       = [(Pawn,   xs)]

 readsPiece' :: ReadS (Piece, Player)
 readsPiece' ('K':xs) = [((King,   White), xs)]
 readsPiece' ('Q':xs) = [((Queen,  White), xs)]
 readsPiece' ('B':xs) = [((Bishop, White), xs)]
 readsPiece' ('N':xs) = [((Knight, White), xs)]
 readsPiece' ('R':xs) = [((Rook,   White), xs)]
 readsPiece' ('P':xs) = [((Pawn,   White), xs)]
 readsPiece' ('k':xs) = [((King,   Black), xs)]
 readsPiece' ('q':xs) = [((Queen,  Black), xs)]
 readsPiece' ('b':xs) = [((Bishop, Black), xs)]
 readsPiece' ('n':xs) = [((Knight, Black), xs)]
 readsPiece' ('r':xs) = [((Rook,   Black), xs)]
 readsPiece' ('p':xs) = [((Pawn,   Black), xs)]
 readsPiece' _ = []

 showsTile :: Tile -> ShowS
 showsTile (f,r) = showsFile f . showsRank r

 readsTile :: ReadS Tile
 readsTile = runReadZ $ liftM2 (,) (ReadZ readsFile) (ReadZ readsRank)

 showsFile :: File -> ShowS
 showsFile f = (toEnum (fromEnum f + 97) :)

 readsFile :: ReadS File
 readsFile [] = []
 readsFile (c:xs) = case toLower c of 'a' -> [(FileA, xs)]
				      'b' -> [(FileB, xs)]
				      'c' -> [(FileC, xs)]
				      'd' -> [(FileD, xs)]
				      'e' -> [(FileE, xs)]
				      'f' -> [(FileF, xs)]
				      'g' -> [(FileG, xs)]
				      'h' -> [(FileH, xs)]
				      _   -> []

 showsRank :: Rank -> ShowS
 showsRank = (:) . intToDigit . succ . fromEnum

 readsRank :: ReadS Rank
 readsRank ('1':xs) = [(Rank1, xs)]
 readsRank ('2':xs) = [(Rank2, xs)]
 readsRank ('3':xs) = [(Rank3, xs)]
 readsRank ('4':xs) = [(Rank4, xs)]
 readsRank ('5':xs) = [(Rank5, xs)]
 readsRank ('6':xs) = [(Rank6, xs)]
 readsRank ('7':xs) = [(Rank7, xs)]
 readsRank ('8':xs) = [(Rank8, xs)]
 readsRank _ = []

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
   -- When promotions are given for non-promotional moves, should the
   -- conversion fail?
   -- When promotions are omitted for promotional moves, should only promotion
   -- to Queen be returned?
  _ -> []

 showsMove :: Move -> ShowS
 showsMove = showsSAN . move2san

 readsMove :: Board -> ReadS Move
 readsMove game str = do (san, r1) <- readsSAN str
			 move <- san2move game san
			 return (move, r1)
