-- | Варіант 16: у вибраному реченні замінити слова заданої довжини
-- вказаним підрядком (його довжина може відрізнятися від довжини слова).
module Textbook.Replace
  ( ReplaceError (..)
  , Replacement (..)
  , describeError
  , replaceInSentence
  , replaceInDocument
  ) where

import Prelude hiding (Word)

import Textbook.Normalize (normalizeWhitespace)
import Textbook.Parser (tokenize)
import Textbook.Types

data ReplaceError
  = NoSentences
  | InvalidSentenceIndex Int Int
    -- ^ запитаний номер і кількість речень
  | InvalidWordLength Int
  deriving (Eq, Show)

-- | Результат заміни.
data Replacement = Replacement
  { replacementDocument :: Document
  , replacementOriginal :: Sentence
  , replacementSentence :: Sentence
  , replacementCount :: Int
  }
  deriving (Eq, Show)

describeError :: ReplaceError -> String
describeError NoSentences = "у тексті немає жодного речення"
describeError (InvalidSentenceIndex i n) =
  "речення з номером " ++ show i ++ " не існує (у тексті речень: " ++ show n
    ++ ", допустимі номери: 1.." ++ show n ++ ")"
describeError (InvalidWordLength n) =
  "довжина слова має бути додатним цілим числом, отримано " ++ show n

-- | Замінює у реченні кожне слово довжини @len@ підрядком @sub@.
-- Підрядок нормалізується (послідовності пробілів і табуляції стають одним
-- пробілом, пробіли на краях відкидаються). Порожній підрядок видаляє слово
-- разом з одним сусіднім пробілом, щоб не лишалося подвійних пробілів чи
-- пробілу перед розділовим знаком. Повертає нове речення і кількість замін.
-- Підставлений текст повторно не переглядається.
replaceInSentence :: Int -> String -> Sentence -> (Sentence, Int)
replaceInSentence len sub (Sentence tokens) = (Sentence newTokens, count)
  where
    replacement = tokenize (normalizeWhitespace sub)
    (newTokens, count) = go [] 0 tokens

    -- acc — вже готові токени у зворотному порядку
    go acc n [] = (reverse acc, n)
    go acc n (TWord w : ts)
      | wordLength w == len = case replacement of
          [] -> case ts of
            TSpace : ts' -> go acc (n + 1) ts'
            _ -> case acc of
              TSpace : acc' -> go acc' (n + 1) ts
              _ -> go acc (n + 1) ts
          _ -> go (reverse replacement ++ acc) (n + 1) ts
    go acc n (t : ts) = go (t : acc) n ts

-- | Заміна у реченні з номером @idx@ (нумерація з 1); інші речення не змінюються.
replaceInDocument :: Int -> Int -> String -> Document -> Either ReplaceError Replacement
replaceInDocument idx len sub (Document sentences)
  | total == 0 = Left NoSentences
  | idx < 1 || idx > total = Left (InvalidSentenceIndex idx total)
  | len < 1 = Left (InvalidWordLength len)
  | otherwise = case splitAt (idx - 1) sentences of
      (before, original : after) ->
        let (changed, count) = replaceInSentence len sub original
         in Right
              Replacement
                { replacementDocument = Document (before ++ changed : after)
                , replacementOriginal = original
                , replacementSentence = changed
                , replacementCount = count
                }
      _ -> Left (InvalidSentenceIndex idx total)
  where
    total = length sentences
