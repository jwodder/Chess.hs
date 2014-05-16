--import Data.List (intercalate)
import Chess
import Chess.Notation

{-
main = mapM_ (putStrLn . ($ "") . showsMove) $ validMoves $ m_after b
 where Just b = domove start (FileE, Rank2) (FileE, Rank4)
-}

main = do
 Just (Move {m_after = b}) <- return $ domove start (FileE,Rank2) (FileE,Rank4)
 --print b
 --print $ pieces b $ b_player b
 --putStrLn $ intercalate ", " [showsPiece' (p, b_player b) $ showsSquare t "" | (p,t) <- pieces b $ b_player b]
 mapM_ (putStrLn . ($ "") . showsMove) $ validMoves b
 --mapM_ (print . move2san) $ validMoves b
