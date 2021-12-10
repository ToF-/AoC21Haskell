module P10
    where

import Data.Maybe
import Data.List

findFirstIllegalChar :: [Char] -> Maybe Char
findFirstIllegalChar = snd . analyse

analyse :: [Char] -> ([Char],Maybe Char)
analyse = foldl checkChunk ("",Nothing)
    where
        checkChunk (stack,Just f) _  = (stack,Just f)
        checkChunk ("",_) c          = (c:"", Nothing)
        checkChunk (stack,_) c | c `elem` "([{<" = (c:stack,Nothing)
        checkChunk ('(':stack,_) ')' = (stack,Nothing)
        checkChunk ('{':stack,_) '}' = (stack,Nothing)
        checkChunk ('[':stack,_) ']' = (stack,Nothing)
        checkChunk ('<':stack,_) '>' = (stack,Nothing)
        checkChunk (stack,_) c       = (stack,Just c)

totalSyntaxErrorScore :: [String] -> Integer
totalSyntaxErrorScore = sum . catMaybes . map ((score <$>) . findFirstIllegalChar)
    where
        score ')' = 3
        score ']' = 57
        score '}' = 1197
        score '>' = 25137

completion :: String -> String
completion s = case analyse s of
                 (_,Just _) -> ""
                 (stack,Nothing) -> reverse (complete stack "")
    where
        complete "" end = end
        complete ('>':'<':stack) end = complete stack end
        complete ('}':'{':stack) end = complete stack end
        complete (']':'[':stack) end = complete stack end
        complete (')':'(':stack) end = complete stack end
        complete ('<':stack) end = complete stack ('>':end)
        complete ('{':stack) end = complete stack ('}':end)
        complete ('[':stack) end = complete stack (']':end)
        complete ('(':stack) end = complete stack (')':end)

completions :: [String] -> [String]
completions = filter (not . null) . map completion

completionScore :: String -> Integer
completionScore = foldl score 0
    where
        score acc ')' = acc * 5 + 1
        score acc ']' = acc * 5 + 2
        score acc '}' = acc * 5 + 3
        score acc '>' = acc * 5 + 4

winner :: [String] -> Integer
winner ss = head $ drop half $ sort $ map completionScore $ cs
    where
        cs = completions ss
        half = (length cs) `div` 2

