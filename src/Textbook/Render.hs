-- | Перетворення моделі назад у текст.
module Textbook.Render
  ( renderToken
  , renderTokens
  , renderSentence
  , renderDocument
  , renderNumbered
  ) where

import Textbook.Types

renderToken :: Token -> String
renderToken (TWord w) = wordText w
renderToken (TPunct p) = [symbolChar (punctuationSymbol p)]
renderToken TSpace = " "

renderTokens :: [Token] -> String
renderTokens = concatMap renderToken

renderSentence :: Sentence -> String
renderSentence = renderTokens . sentenceTokens

-- | Речення розділяються одним пробілом.
renderDocument :: Document -> String
renderDocument = unwords . map renderSentence . documentSentences

-- | Нумерований список речень (нумерація з 1).
renderNumbered :: Document -> String
renderNumbered (Document ss) =
  unlines [show i ++ ". " ++ renderSentence s | (i, s) <- zip [1 :: Int ..] ss]
