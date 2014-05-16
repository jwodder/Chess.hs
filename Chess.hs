module Chess (
  -- * Players
  Player(..), vs, home, pawnwards,
  -- * Pieces
  Piece(..), pieces,
  -- * Tiles
  Tile, File(..), Rank(..),
  tile2xy, xy2tile,
  -- * Game states
  Board(..),
  start,
  inCheck, checkmate, stalemate,
  getTile, isEmpty,
  boardStates,
  -- * Moving
  Move(..), setPromotion,
  domove, shortCastle, longCastle, pieceTo,
  validMoves, movesFrom, movesTo,
  -- * Detecting attacks
  -- ** Attacks
  -- |For the purposes of the following functions, a piece is said to /attack/
  -- a tile iff it is capable of capturing a piece on that tile in a single
  -- move (regardless of whether the tile is currently occupied), ignoring
  -- whether such a capture would leave the piece's king in check and ignoring
  -- the possibility of capturing /en passant/.  This definition is influenced
  -- by item 3.1 of the FIDE Laws of Chess
  -- <http://www.fide.com/component/handbook/?id=124&view=article> and is used
  -- in determining check and checkmate.
  attacks, attacksTiles, attackedBy, playerAttacks,
  -- ** Captures
  -- |As the \"Attacks\" functions, but the \"no leaving your king in check\"
  -- rule is taken into account (/En passant/ is still ignored).  Note that
  -- these functions ignore whether and by what pieces the \"captured\" tiles
  -- are currently occupied.
  canCapture, canCaptureTiles, capturableBy,
 ) where
 import Control.Monad (guard)
 import Data.Array
 import Data.Maybe (fromMaybe, isNothing, isJust)
 import Chess.Util

 data Player = White | Black deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 data Piece = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 data File = FileA | FileB | FileC | FileD | FileE | FileF | FileG | FileH
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 data Rank = Rank1 | Rank2 | Rank3 | Rank4 | Rank5 | Rank6 | Rank7 | Rank8
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Ix)

 type Tile = (File, Rank)

 data Board = Board {
  b_player      :: Player,
   -- ^The current player (i.e., the player who is to make the next move)
  b_board       :: Array Tile (Maybe (Piece, Player)),  -- ^The board
  b_canCastleWK :: Bool,
   -- ^Whether white can castle kingside (i.e., whether the king and his rook
   -- have yet to move)
  b_canCastleWQ :: Bool,   -- ^Whether white can castle queenside
  b_canCastleBK :: Bool,   -- ^Whether black can castle kingside
  b_canCastleBQ :: Bool,   -- ^Whether black can castle queenside
  b_passant     :: Maybe File
   -- ^The file of a pawn that moved two spaces on the previous move, if any
 } deriving (Eq, Ord, Read, Show)

 data Move = Move {
  -- Castling is represented by the relevant movement of the king.
  m_before        :: Board,  -- ^The board state immediately before the move
  m_after         :: Board,  -- ^The board state immediately after the move
  m_player        :: Player,       -- ^The player who moved
  m_piece         :: Piece,        -- ^The piece moved
  m_from          :: Tile,         -- ^The starting tile of the moved piece
  m_to            :: Tile,         -- ^The ending tile of the moved piece
  m_captured      :: Maybe Piece,  -- ^The type of piece captured, if any
  m_promoted      :: Maybe Piece,
   -- ^The type of piece to which a pawn was promoted, if any
  m_isShortCastle :: Bool,  -- ^Whether the move was a short/kingside castle
  m_isLongCastle  :: Bool,  -- ^Whether the move was a long/queenside castle
  m_isEnPassant   :: Bool,  -- ^Whether the move was an /en passant/ capture
  m_checks        :: Bool,
   -- ^Whether the move put the opponent in check (but not checkmate)
  m_checkmates    :: Bool   -- ^Whether the move checkmated the opponent
 } deriving (Eq, Ord, Read, Show)

 -- |Set the type of piece to which a pawn is promoted.  If the move does not
 -- involve promotion, or if an invalid piece (i.e., 'Pawn' or 'King') is
 -- supplied, the original move is returned unchanged.  Note that moves
 -- returned by 'domove' always promote pawns to 'Queen', while the other
 -- move-generating functions return separate moves for each possible
 -- promotion.
 setPromotion :: Move -> Piece -> Move
 setPromotion m@(Move {m_promoted = Nothing}) _ = m
 setPromotion m Pawn = m
 setPromotion m King = m
 setPromotion m p = m {m_promoted = Just p,
		       m_after = (m_after m) {b_board = b_board (m_after m) //
					      [(m_to m, Just (p, m_player m))]}}

 getTile :: Board -> Tile -> Maybe (Piece, Player)
 getTile = (!) . b_board

 -- |Tests whether the given 'Tile' on the given 'Board' is unoccupied
 isEmpty :: Board -> Tile -> Bool
 isEmpty game = isNothing . getTile game

 -- |@pieces b player@ returns a list of all pieces (and their locations) on
 -- board @b@ owned by @player@.
 pieces :: Board -> Player -> [(Piece, Tile)]
 pieces game s = [(p,t) | (t, Just (p,s')) <- assocs $ b_board game, s == s']

 tile2xy :: Tile -> (Int, Int)
 tile2xy (f, r) = (fromEnum f, fromEnum r)

 xy2tile :: (Int, Int) -> Tile
 xy2tile (x, y) = (toEnum x, toEnum y)

 -- |Returns the list of board states that exist during a sequence of moves,
 -- including the beginning and ending states.  This function assumes that the
 -- moves given are all consecutive &#x2014; specifically, that for every move
 -- @m1@ that is immediately followed by a move @m2@, @m_after m1 == m_before
 -- m2@.
 boardStates :: [Move] -> [Board]
 boardStates (m:ms) = m_before m : map m_after ms
 boardStates [] = []

 -- |Returns the opponent of the given 'Player', i.e., the 'Player' who is not
 -- the given 'Player'
 vs :: Player -> Player
 vs White = Black
 vs Black = White

 -- |Returns the home rank of the given 'Player' ('Rank1' for 'White', 'Rank8'
 -- for 'Black')
 home :: Player -> Rank
 home White = Rank1
 home Black = Rank8

 pawnwards :: Player -> Rank -> Rank
 pawnwards White = succ
 pawnwards Black = pred

 -- |The starting state of a game of chess
 start :: Board
 start = Board {
  b_player = White,
  b_board = accumArray (\_ x -> Just x) Nothing (minBound, maxBound)
   $ concat [[((f, home side), (piece, side)),
	      ((f, pawnwards side $ home side), (Pawn, side))]
	     | (f, piece) <- zip [FileA .. FileH] [Rook, Knight, Bishop, Queen,
						   King, Bishop, Knight, Rook],
	       side <- [White, Black]],
  b_canCastleWK = True,
  b_canCastleWQ = True,
  b_canCastleBK = True,
  b_canCastleBQ = True,
  b_passant = Nothing
 }

 domove :: Board -> Tile -> Tile -> Maybe Move
 domove game from@(f1, r1) to@(f2, r2) = do
  (piece, side) <- getTile game from
  guard $ side == b_player game
  capt <- case getTile game to of Just (p,s) | s /= side -> return $ Just p
					     | otherwise -> Nothing
				  Nothing                -> return Nothing
  (capt', ep) <- if piece == Pawn && f1 /= f2 && capt == Nothing
		 then do (Pawn, s) <- getTile game (f2, r1)
			 guard $ s /= side && b_passant game == Just f2
			 return (Just Pawn, True)
		 else return (capt, False)
  let promo = guard (piece == Pawn && r2 == home (vs side)) >> return Queen
      is00  = (piece, f1, f2) == (King, FileE, FileG)
      is000 = (piece, f1, f2) == (King, FileE, FileC)
      wkr = (FileH, Rank1)
      wqr = (FileA, Rank1)
      bkr = (FileH, Rank8)
      bqr = (FileA, Rank8)
      after = Board {
	b_player = vs side,
	b_board = b_board game //
	 ([(from, Nothing), (to, Just (fromMaybe piece promo, side))]
	  ++ (ep    ?: [((f2, r1), Nothing)] :? [])
	  ++ (is00  ?: [((FileH, home side), Nothing),
			((FileF, home side), Just (Rook, side))] :? [])
	  ++ (is000 ?: [((FileA, home side), Nothing),
			((FileD, home side), Just (Rook, side))] :? [])),
	b_canCastleWK = b_canCastleWK game && to /= wkr &&
	 not (side == White && (piece == King || (piece, from) == (Rook, wkr))),
	b_canCastleWQ = b_canCastleWQ game && to /= wqr &&
	 not (side == White && (piece == King || (piece, from) == (Rook, wqr))),
	b_canCastleBK = b_canCastleBK game && to /= bkr &&
	 not (side == Black && (piece == King || (piece, from) == (Rook, bkr))),
	b_canCastleBQ = b_canCastleBQ game && to /= bqr &&
	 not (side == Black && (piece == King || (piece, from) == (Rook, bqr))),
	b_passant = case (piece, from, to) of
			 (Pawn, (f, Rank2), (_, Rank4)) -> Just f
			 (Pawn, (f, Rank7), (_, Rank5)) -> Just f
			 _                              -> Nothing
       }
  let (x1, y1) = tile2xy from
      (x2, y2) = tile2xy to
      δ        = (abs $ x2-x1, abs $ y2-y1)
      (able, mid, betwixt) = is00
       ?: (side == White ?: b_canCastleWK :? b_canCastleBK,
	   (FileF, home side),
	   [(f, home side) | f <- [FileF .. FileG]])
       :? (side == White ?: b_canCastleWQ :? b_canCastleBQ,
	   (FileD, home side),
	   [(f, home side) | f <- [FileB .. FileD]])
  guard $ case piece of
   Pawn -> (side == White ?: (>) :? (<)) y2 y1
	    && case δ of (0,1) -> isNothing capt'
			 (0,2) -> r1 `elem` [Rank2, Rank7]
				   && isEmpty game (f1, pawnwards side r1)
				   && isNothing capt'
			 (1,1) -> isJust capt'
			 _     -> False
   King -> elem δ [(0,1), (1,0), (1,1)]
	    || δ == (2,0) && from == (FileE, home side)
			  && not (inCheck game)
			  && able game
			  && not (playerAttacks game (vs side) mid)
			  && all (isEmpty game) betwixt
   _ -> attacks game from to
  guard  $ not $ inCheck' after side
  return $ Move {m_before        = game,
		 m_after         = after,
		 m_player        = side,
		 m_piece         = piece,
		 m_from          = from,
		 m_to            = to,
		 m_captured      = capt',
		 m_promoted      = promo,
		 m_isShortCastle = is00,
		 m_isLongCastle  = is000,
		 m_isEnPassant   = ep,
		 m_checks        = inCheck after && not (checkmate after),
		 m_checkmates    = checkmate after}

 -- |@attacks b t1 t2@ tests whether there is currently a piece on tile @t1@
 -- that is attacking tile @t2@ on board @b@.
 attacks :: Board -> Tile -> Tile -> Bool
 attacks game from to = δ' /= (0,0) && case getTile game from of
  Just (Pawn, side) -> sy == (side == White ?: 1 :? (-1)) && δ' == (1,1)
  Just (Rook,    _) -> (δx == 0 || δy == 0) && clear
  Just (Knight,  _) -> δ' == (1,2) || δ' == (2,1)
  Just (Bishop,  _) -> δx' == δy' && clear
  Just (Queen,   _) -> (δx == 0 || δy == 0 || δx' == δy') && clear
  Just (King,    _) -> elem δx' [0,1] && elem δy' [0,1]
  Nothing           -> False
  where (x1, y1) = tile2xy from
	(x2, y2) = tile2xy to
	(δx, δy) = (x2 - x1, y2 - y1)
	(sx, sy) = (signum δx, signum δy)
	δ'@(δx', δy') = (abs δx, abs δy)
	clear = all (isEmpty game . xy2tile) $ takeWhile (/= (x2, y2))
		 $ iterate (\(a,b) -> (a+sx, b+sy)) (x1+sx, y1+sy)

 -- |@playerAttacks b side tile@ tests whether any of @side@'s pieces are
 -- currently attacking tile @tile@ on board @b@.
 playerAttacks :: Board -> Player -> Tile -> Bool
 playerAttacks game side t = or [attacks game t2 t | (_,t2) <- pieces game side]

 -- |@attacksTiles b tile@ returns a list of all tiles that the piece at tile
 -- @tile@ is currently attacking on board @b@.  If there is no piece at
 -- @tile@, @[]@ is returned.
 attacksTiles :: Board -> Tile -> [Tile]
 attacksTiles game t = filter (attacks game t) $ range (minBound, maxBound)

 -- |@attackedBy b tile@ returns a list of all pieces (including their tiles
 -- and owners) that are currently attacking tile @tile@ on board @b@.
 attackedBy :: Board -> Tile -> [(Tile, (Piece, Player))]
 attackedBy game t = do (t2, Just pp) <- assocs $ b_board game
			guard $ attacks game t2 t
			return (t2, pp)

 canCapture :: Board -> Tile -> Tile -> Bool
 canCapture game from to = case getTile game from of
  p@(Just (_, s)) -> attacks game from to
		      && not (inCheck' (game {b_board = b_board game //
					       [(from, Nothing), (to, p)]}) s)
  _ -> False

 -- |@canCaptureTiles b tile@ returns a list of all tiles on board @b@ which
 -- the piece at tile @tile@ currently threatens with capture.  If there is no
 -- piece at @tile@, @[]@ is returned.
 canCaptureTiles :: Board -> Tile -> [Tile]
 canCaptureTiles game t = filter (canCapture game t) $ range (minBound,maxBound)

 -- |@capturableBy b tile@ returns a list of all pieces (including their tiles
 -- and owners) that can currently capture an arbitrary piece on tile @tile@ on
 -- board @b@.
 capturableBy :: Board -> Tile -> [(Tile, (Piece, Player))]
 capturableBy game t = do (t2, Just pp) <- assocs $ b_board game
			  guard $ canCapture game t2 t
			  return (t2, pp)

 -- |@movesFrom b tile@ returns a list of all possible legal moves that the
 -- current player can make on board @b@ by moving the piece at tile @tile@.
 -- If there is no piece at @tile@, @[]@ is returned.
 movesFrom :: Board -> Tile -> [Move]
 movesFrom game from = do to     <- range (minBound, maxBound)
			  Just m <- return $ domove game from to
			  promos m

 -- |@movesTo b tile@ returns a list of all possible legal moves that the
 -- current player can make on board @b@ by moving a piece to tile @tile@.
 movesTo :: Board -> Tile -> [Move]
 movesTo game to = do from   <- range (minBound, maxBound)
		      Just m <- return $ domove game from to
		      promos m

 -- |@validMoves b@ returns a list of all possible legal moves of the current
 -- player on board @b@.
 validMoves :: Board -> [Move]
 validMoves g = movesFrom g . snd =<< pieces g (b_player g)

 -- |@pieceTo b piece tile@ returns a list of all possible moves on board @b@
 -- that consist of the current player moving a @piece@ to @tile@.  It is
 -- useful for interpreting moves written in algebraic notation.
 pieceTo :: Board -> Piece -> Tile -> [Move]
 pieceTo game piece dest = do (p,t) <- pieces game $ b_player game
			      guard $ p == piece
			      Just m <- return $ domove game t dest
			      promos m

 shortCastle :: Board -> Maybe Move
 shortCastle game = do let home' = home $ b_player game
		       (King, _) <- getTile game (FileE, home')
		       domove game (FileE, home') (FileG, home')

 longCastle :: Board -> Maybe Move
 longCastle game = do let home' = home $ b_player game
		      (King, _) <- getTile game (FileE, home')
		      domove game (FileE, home') (FileC, home')

 -- |Tests whether the current player is in check
 inCheck :: Board -> Bool
 inCheck game = inCheck' game $ b_player game

 -- |Tests whether the current player has been checkmated
 checkmate :: Board -> Bool
 checkmate game = inCheck game && null (validMoves game)

 -- |Tests whether the game has ended in stalemate
 stalemate :: Board -> Bool
 stalemate game = not (inCheck game) && null (validMoves game)

 inCheck' :: Board -> Player -> Bool  -- not exported
 inCheck' game side = or $ do (King, kt) <- pieces game side
			      (_,    t2) <- pieces game $ vs side
			      return $ attacks game t2 kt

 promos :: Move -> [Move]  -- not exported
 promos m@(Move {m_promoted = Nothing}) = [m]
 promos m = map (setPromotion m) [Rook .. Queen]
