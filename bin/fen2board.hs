import Data.Array
import Data.List (intercalate)
import System.Environment (getArgs)

import Chess
import Chess.Notation (showsFile, showsRank)
import Chess.Notation.FEN (readsFEN)

trule, hrule, brule :: String
trule = "  ┌─┬─┬─┬─┬─┬─┬─┬─┐"
hrule = "  ├─┼─┼─┼─┼─┼─┼─┼─┤"
brule = "  └─┴─┴─┴─┴─┴─┴─┴─┘"

figures :: Array (Piece, Player) Char
figures = array (minBound, maxBound) [
    ((King,   White), '♔'),    ((King,   Black), '♚'),
    ((Queen,  White), '♕'),    ((Queen,  Black), '♛'),
    ((Rook,   White), '♖'),    ((Rook,   Black), '♜'),
    ((Bishop, White), '♗'),    ((Bishop, Black), '♝'),
    ((Knight, White), '♘'),    ((Knight, Black), '♞'),
    ((Pawn,   White), '♙'),    ((Pawn,   Black), '♟')
 ]

main :: IO ()
main = do argv <- getArgs
	  input <- case argv of []    -> getContents
	  			"-":_ -> getContents
				x  :_ -> readFile x
	  mapM_ (\fen -> do
	   board <- case [b | ((b,_,_),r) <- readsFEN fen, ("", "") <- lex r] of
		     [b] -> return b
		     _   -> fail $ "Could not parse line: "  ++ fen
	   putStrLn trule
	   mapM_ (\r -> do
	       putStrLn $ showsRank r $ ' ' : '│'
		: intercalate "│" [[showTile board (f,r)]
				   | f <- [FileA .. FileH]] ++ "│"
	       putStrLn (if r == Rank1 then brule else hrule)
	    ) [Rank8, Rank7 .. Rank1]
	   putStrLn $ "   " ++ intercalate " " [showsFile f ""
						| f <- [FileA .. FileH]]
	   putChar '\n') $ filter (not . null) $ lines input

showTile :: Board -> Square -> Char
showTile b sq@(r,f) = case getSquare b sq of
    Just p  -> figures ! p
    Nothing -> if even (fromEnum r) == even (fromEnum f) then '█' else ' '
