module MoreStrings where
 import Data.Char

 -- TODO: Make wrapLine handle tabs and newlines correctly without having to
 -- call wrapLines
 -- TODO: Make wrapLine handle hyphens & soft hyphens?
 wrapLine :: Int -> String -> [String]
 wrapLine _ [] = []
 wrapLine len str = first : wrapLine len rest
  where (first, rest) = wrap' 0 0 str
	wrap' _ _ [] = (str, [])
	wrap' lst trail s = if lst > 0 && lst + trail + length a > len
			    then (take lst str, drop (lst + trail) str)
			    else wrap' (lst+trail+length a) (length ws) b'
	 where (a, b) = span (not . breakable) s
	       (ws, b') = span breakable b
	breakable c = isSpace c && c /= '\xA0'

 horizSpace :: Char -> Bool
 horizSpace '\t' = True
 horizSpace c    = generalCategory c == Space
