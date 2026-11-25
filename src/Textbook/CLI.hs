-- | Розбір аргументів командного рядка.
module Textbook.CLI
  ( Command (..)
  , Options (..)
  , emptyOptions
  , parseArgs
  , usageText
  ) where

import Data.List (stripPrefix)
import Text.Read (readMaybe)

data Options = Options
  { optFile :: Maybe FilePath
  , optSentence :: Maybe Int
  , optLength :: Maybe Int
  , optReplacement :: Maybe String
  , optList :: Bool
  , optTextOnly :: Bool
  }
  deriving (Eq, Show)

data Command
  = ShowHelp
  | Run Options
  deriving (Eq, Show)

emptyOptions :: Options
emptyOptions = Options Nothing Nothing Nothing Nothing False False

parseArgs :: [String] -> Either String Command
parseArgs args
  | any (`elem` ["-h", "--help"]) args = Right ShowHelp
  | otherwise = Run <$> go emptyOptions args

go :: Options -> [String] -> Either String Options
go o [] = Right o
go o (a : rest)
  | a == "--list" = go o {optList = True} rest
  | a == "--text-only" = go o {optTextOnly = True} rest
  | a `elem` ["-s", "--sentence"] = withValue a rest $ \v r -> do
      n <- parseInt a v
      go o {optSentence = Just n} r
  | a `elem` ["-l", "--length"] = withValue a rest $ \v r -> do
      n <- parseInt a v
      go o {optLength = Just n} r
  | a `elem` ["-r", "--replace"] = withValue a rest $ \v r ->
      go o {optReplacement = Just v} r
  | Just v <- stripPrefix "--sentence=" a = parseInt "--sentence" v >>= \n -> go o {optSentence = Just n} rest
  | Just v <- stripPrefix "--length=" a = parseInt "--length" v >>= \n -> go o {optLength = Just n} rest
  | Just v <- stripPrefix "--replace=" a = go o {optReplacement = Just v} rest
  | a == "--" = foldl (\acc f -> acc >>= (`addFile` f)) (Right o) rest
  | take 1 a == "-" && a /= "-" = Left ("невідома опція «" ++ a ++ "»")
  | otherwise = addFile o a >>= \o' -> go o' rest

-- Значення опції береться безумовно, навіть якщо починається з «-».
withValue :: String -> [String] -> (String -> [String] -> Either String a) -> Either String a
withValue _ (v : r) k = k v r
withValue name [] _ = Left ("опція " ++ name ++ " потребує значення")

parseInt :: String -> String -> Either String Int
parseInt name v = case readMaybe v of
  Just n -> Right n
  Nothing -> Left ("значення «" ++ v ++ "» опції " ++ name ++ " не є цілим числом")

addFile :: Options -> FilePath -> Either String Options
addFile o f = case optFile o of
  Nothing -> Right o {optFile = Just f}
  Just _ -> Left "можна вказати лише один вхідний файл"

usageText :: String
usageText =
  unlines
    [ "Використання: textbook-word-replacer [ОПЦІЇ] [ФАЙЛ]"
    , ""
    , "У реченні з номером N замінює всі слова довжини L підрядком S."
    , "Параметри, яких не вказано, програма запитує інтерактивно."
    , ""
    , "Опції:"
    , "  -s, --sentence N   номер речення (з 1)"
    , "  -l, --length L     довжина слів, що замінюються"
    , "  -r, --replace S    підрядок-заміна (може бути порожнім: --replace=)"
    , "      --list         показати пронумеровані речення й вийти"
    , "      --text-only    вивести лише змінений текст"
    , "  -h, --help         ця довідка"
    ]
