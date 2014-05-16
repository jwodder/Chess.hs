import Data.Bits
import System.Environment (getArgs)
import Chess
import Chess.Notation (showsPiece')
import Chess.Notation.PGN

main = getArgs >>= mapM_ (\file -> do
 [((_, moves, _), _)] <- return . readsPGNGame =<< readFile file
 mapM_ (printBoard . m_after . last) $ pgn2moves start moves)

printBoard :: Board -> IO ()
printBoard b = putStrLn inter >> mapM_ (\r -> putStrLn $ '|' : concat
 [maybe (sp:sp:"|") (($ sp:"|") . showsPiece') $ getSquare b (f,r)
  | f <- [FileA .. FileH],
  let sp = if (fromEnum r `xor` fromEnum f) .&. 1 == 0 then '▒' else ' ']
 ++ '\n' : inter) [Rank8, Rank7 .. Rank1] >> putChar '\n'
 where inter = "+--+--+--+--+--+--+--+--+"
