import System.Environment (getArgs)
import Chess.Notation.PGN

main = getArgs >>= mapM_ (\file -> do
 [(g, _)] <- return . readsPGNGame =<< readFile file
 putStr $ showsPGNGame g "")
