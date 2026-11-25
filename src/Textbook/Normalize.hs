-- | Нормалізація пробілів.
module Textbook.Normalize
  ( normalizeWhitespace
  ) where

import Data.Char (isSpace)

-- | Кожна послідовність пробільних символів (пробіли, табуляції, переноси
-- рядків, нерозривний пробіл) стає одним пробілом; пробіли на краях
-- видаляються.
normalizeWhitespace :: String -> String
normalizeWhitespace = unwords . words'
  where
    words' s = case dropWhile isSpace s of
      "" -> []
      s' -> let (w, rest) = break isSpace s' in w : words' rest
