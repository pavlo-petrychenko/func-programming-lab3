-- | Розбір тексту на токени та речення.
--
-- Слово: послідовність літер (кирилиця, латиниця, інші), цифр і @_@. Усередині
-- слова допустимі апостроф (@'@, @’@, @ʼ@) і дефіс між літерами/цифрами, а також
-- @.@ або @,@ між цифрами (десяткові числа: @3.14@, @1,5@).
--
-- Кінець речення: підряд знаків @. ! ? …@ (і закривальні лапки/дужки одразу
-- після них), за якими йде пробіл або кінець тексту. Крапка не завершує речення,
-- якщо перед нею скорочення (@напр.@, @рис.@, @Dr.@) або одна велика літера
-- (ініціал), а також якщо після крапки / трикрапки наступне слово починається
-- з малої літери (@т. д. і т. п.@).
module Textbook.Parser
  ( parseText
  , tokenize
  , splitSentences
  , isAbbreviation
  ) where

import Data.Char (isAlphaNum, isDigit, isLower, isSpace, isUpper, toLower)
import Prelude hiding (Word)

import Textbook.Normalize (normalizeWhitespace)
import Textbook.Types

-- | Повний розбір: нормалізація пробілів, токенізація, поділ на речення.
parseText :: String -> Document
parseText = Document . splitSentences . tokenize . normalizeWhitespace

------------------------------------------------------------------------------
-- Токенізація

-- | Розбиває рядок на слова, розділові знаки та пробіли. Послідовність
-- пробільних символів дає один 'TSpace'.
tokenize :: String -> [Token]
tokenize [] = []
tokenize s@(c : cs)
  | isSpace c = TSpace : tokenize (dropWhile isSpace cs)
  | isWordChar c =
      let (w, rest) = spanWord s
       in TWord (mkWord w) : tokenize rest
  | otherwise = TPunct (classifyPunctuation c) : tokenize cs

isWordChar :: Char -> Bool
isWordChar c = isAlphaNum c || c == '_'

isApostrophe :: Char -> Bool
isApostrophe c = c `elem` "'’ʼ"

-- | Чи може @j@ стояти всередині слова між символами @a@ та @b@.
isJoiner :: Char -> Char -> Char -> Bool
isJoiner a j b
  | isApostrophe j || j == '-' = isWordChar a && isWordChar b
  | j == '.' || j == ',' = isDigit a && isDigit b
  | otherwise = False

-- | Читає слово, що починається із символу слова.
spanWord :: String -> (String, String)
spanWord [] = ([], [])
spanWord (c : rest) = case rest of
  (j : n : more)
    | isJoiner c j n ->
        let (w, r) = spanWord (n : more)
         in (c : j : w, r)
  (n : _)
    | isWordChar n ->
        let (w, r) = spanWord rest
         in (c : w, r)
  _ -> ([c], rest)

classifyPunctuation :: Char -> Punctuation
classifyPunctuation c = Punctuation kind (Symbol c)
  where
    kind
      | c `elem` ".!?…" = Terminal
      | c `elem` "([{«„“" = Opening
      | c `elem` ")]}»”" = Closing
      | c `elem` "\"'’‘`" = Quote
      | otherwise = Inner

------------------------------------------------------------------------------
-- Поділ на речення

-- | Ділить потік токенів на речення. Пробіл між реченнями відкидається.
splitSentences :: [Token] -> [Sentence]
splitSentences = go []
  where
    -- cur — токени поточного речення у зворотному порядку
    go cur [] = emit cur []
    go cur (t : ts)
      | isTerminal t =
          let (run, afterRun) = span isTerminal (t : ts)
              (closers, rest) = span isCloser afterRun
              cur' = reverse (run ++ closers) ++ cur
           in if endsSentence cur run rest
                then emit cur' (go [] (dropSpace rest))
                else go cur' rest
      | otherwise = go (t : cur) ts

    emit cur next = case trim (reverse cur) of
      [] -> next
      toks -> Sentence toks : next

    dropSpace (TSpace : r) = r
    dropSpace r = r

    trim = reverse . dropWhile isSpaceToken . reverse . dropWhile isSpaceToken

isTerminal :: Token -> Bool
isTerminal (TPunct (Punctuation Terminal _)) = True
isTerminal _ = False

isCloser :: Token -> Bool
isCloser (TPunct (Punctuation k _)) = k == Closing || k == Quote
isCloser _ = False

terminalChar :: Token -> Char
terminalChar (TPunct (Punctuation _ (Symbol c))) = c
terminalChar _ = ' '

-- | Чи завершує серія термінальних знаків @run@ речення.
-- @cur@ — попередні токени (у зворотному порядку), @rest@ — токени після
-- знаків і закривальних лапок.
endsSentence :: [Token] -> [Token] -> [Token] -> Bool
endsSentence cur run rest = case rest of
  [] -> True
  TSpace : more ->
    let chars = map terminalChar run
        onlyDots = all (`elem` ".…") chars
        singleDot = chars == "."
     in not (onlyDots && startsLower more)
          && not (singleDot && abbreviationBefore cur)
  _ -> False -- немає пробілу: «foo.bar», «a.b»
  where
    startsLower (TWord w : _) = case wordText w of
      (x : _) -> isLower x
      [] -> False
    startsLower _ = False

-- | Останнє слово перед крапкою — скорочення або ініціал?
abbreviationBefore :: [Token] -> Bool
abbreviationBefore (TWord w : _) =
  let s = wordText w
   in isAbbreviation s || (length s == 1 && all isUpper s)
abbreviationBefore _ = False

-- | Відомі скорочення (без крапки), регістр не важливий.
isAbbreviation :: String -> Bool
isAbbreviation s = map toLower s `elem` abbreviations

abbreviations :: [String]
abbreviations =
  [ "напр", "див", "рис", "табл", "стор", "ім", "проф", "доц", "акад", "ін", "пр"
  , "mr", "mrs", "ms", "dr", "prof", "fig", "vs", "approx", "sec", "ch"
  ]
