import System.Environment (getArgs)
import Chess (start)
import Chess.Notation.FEN
import Chess.Notation.PGN
import MoreIO (readArgv)

main = getArgs >>= readArgv >>= mapM_ (\str -> do
 [((_, moves, _), _)] <- return $ readsPGNGame str
 mapM_ (putStrLn . ($ "") . showsFEN . finalFEN) $ pgn2moves start moves)
