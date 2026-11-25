-- | Модель тексту: символ, слово, розділовий знак, токен, речення, документ.
module Textbook.Types
  ( Symbol (..)
  , Word (..)
  , mkWord
  , wordText
  , wordLength
  , PunctuationKind (..)
  , Punctuation (..)
  , Token (..)
  , Sentence (..)
  , Document (..)
  , sentenceWords
  , documentWords
  , isWordToken
  , isSpaceToken
  ) where

import Prelude hiding (Word)

-- | Один символ вхідного тексту.
newtype Symbol = Symbol {symbolChar :: Char}
  deriving (Eq, Ord, Show)

-- | Слово — непорожня послідовність символів без пробілів.
newtype Word = Word {wordSymbols :: [Symbol]}
  deriving (Eq, Ord, Show)

-- | Вид розділового знака.
data PunctuationKind
  = Terminal  -- ^ кінець речення: @. ! ? …@
  | Opening   -- ^ відкривальна дужка або лапка: @( [ { « „ “@
  | Closing   -- ^ закривальна дужка або лапка: @) ] } » ”@
  | Quote     -- ^ нейтральна лапка: @" ' ’ ‘@
  | Inner     -- ^ усі інші знаки: кома, двокрапка, тире, @+@, @=@ тощо
  deriving (Eq, Ord, Show)

-- | Розділовий знак — один символ із позначеним видом.
data Punctuation = Punctuation
  { punctuationKind :: PunctuationKind
  , punctuationSymbol :: Symbol
  }
  deriving (Eq, Ord, Show)

-- | Елемент речення: слово, розділовий знак або (єдиний) пробіл.
data Token
  = TWord Word
  | TPunct Punctuation
  | TSpace
  deriving (Eq, Ord, Show)

-- | Речення — послідовність токенів без пробілів на краях.
newtype Sentence = Sentence {sentenceTokens :: [Token]}
  deriving (Eq, Ord, Show)

-- | Текст — послідовність речень.
newtype Document = Document {documentSentences :: [Sentence]}
  deriving (Eq, Ord, Show)

mkWord :: String -> Word
mkWord = Word . map Symbol

wordText :: Word -> String
wordText (Word symbols) = map symbolChar symbols

-- | Довжина слова — кількість його символів (апостроф і дефіс рахуються).
wordLength :: Word -> Int
wordLength (Word symbols) = length symbols

isWordToken :: Token -> Bool
isWordToken (TWord _) = True
isWordToken _ = False

isSpaceToken :: Token -> Bool
isSpaceToken TSpace = True
isSpaceToken _ = False

sentenceWords :: Sentence -> [Word]
sentenceWords (Sentence tokens) = [w | TWord w <- tokens]

documentWords :: Document -> [Word]
documentWords (Document sentences) = concatMap sentenceWords sentences
