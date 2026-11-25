module Main (main) where

import Control.Monad (unless)
import Data.Char (chr, isDigit)
import Data.IORef
import System.Exit (exitFailure)
import Prelude hiding (Word)

import Textbook.CLI
import Textbook.Normalize
import Textbook.Parser
import Textbook.Render
import Textbook.Replace
import Textbook.Types

------------------------------------------------------------------------------
-- Мінімальний тестовий каркас

type Test = (String, Bool)

check :: (Eq a, Show a) => String -> a -> a -> Test
check name actual expected = (name ++ detail, actual == expected)
  where
    detail
      | actual == expected = ""
      | otherwise = "\n      очікувалось: " ++ unescape (show expected) ++ "\n      отримано:    " ++ unescape (show actual)

-- | Повертає кирилицю в виводі 'show' (замість \\1053 тощо).
unescape :: String -> String
unescape ('\\' : c : cs)
  | isDigit c = let (ds, r) = span isDigit (c : cs) in chr (read ds) : unescape r
  | otherwise = '\\' : c : unescape cs
unescape (c : cs) = c : unescape cs
unescape [] = []

-- Допоміжні функції --------------------------------------------------------

sentencesOf :: String -> [String]
sentencesOf = map renderSentence . documentSentences . parseText

wordsOf :: String -> [String]
wordsOf = map wordText . documentWords . parseText

-- | Заміна у реченні номер i; повертає змінений текст усього документа.
replaceText :: Int -> Int -> String -> String -> Either ReplaceError String
replaceText i len sub txt =
  renderDocument . replacementDocument <$> replaceInDocument i len sub (parseText txt)

sentenceAfter :: Int -> Int -> String -> String -> Either ReplaceError String
sentenceAfter i len sub txt =
  renderSentence . replacementSentence <$> replaceInDocument i len sub (parseText txt)

sample :: String
sample = "Один два три. Кіт і пес! Чи є кіт?"

------------------------------------------------------------------------------
-- Тести

normalizeTests :: [Test]
normalizeTests =
  [ check "табуляція -> пробіл" (normalizeWhitespace "a\tb") "a b"
  , check "послідовність пробілів -> один" (normalizeWhitespace "a     b") "a b"
  , check "змішані пробіли й табуляції" (normalizeWhitespace "a \t \t  b") "a b"
  , check "пробіли на краях видаляються" (normalizeWhitespace "  a b \t") "a b"
  , check "переноси рядків і CRLF" (normalizeWhitespace "a\r\n\r\n b\nc") "a b c"
  , check "порожній рядок" (normalizeWhitespace "") ""
  , check "лише пробіли" (normalizeWhitespace " \t \n ") ""
  , check "кирилиця не псується" (normalizeWhitespace "Привіт,\t\tсвіте!") "Привіт, світе!"
  ]

parserTests :: [Test]
parserTests =
  [ check "слова та знаки" (wordsOf "Hello, world!") ["Hello", "world"]
  , check "кириличні слова" (wordsOf "Мова Haskell — функціональна.") ["Мова", "Haskell", "функціональна"]
  , check "апостроф у слові" (wordsOf "Не обов'язково, м’яч.") ["Не", "обов'язково", "м’яч"]
  , check "дефіс у слові" (wordsOf "по-перше - так") ["по-перше", "так"]
  , check "десяткове число — одне слово" (wordsOf "Число 3.14 і 1,5.") ["Число", "3.14", "і", "1,5"]
  , check "ідентифікатор із підкресленням" (wordsOf "змінна my_value_2 тут") ["змінна", "my_value_2", "тут"]
  , check "довжина слова з апострофом" (map wordLength (documentWords (parseText "м'яч"))) [4]
  , check "тип знака: кінець речення"
      (tokenize "a!") [TWord (mkWord "a"), TPunct (Punctuation Terminal (Symbol '!'))]
  , check "тип знака: дужки та кома"
      [punctuationKind p | TPunct p <- tokenize "(a, b)"] [Opening, Inner, Closing]
  , check "пробіли -> один TSpace" (tokenize "a \t  b") [TWord (mkWord "a"), TSpace, TWord (mkWord "b")]
  , check "три речення" (sentencesOf sample) ["Один два три.", "Кіт і пес!", "Чи є кіт?"]
  , check "трикрапка в кінці" (sentencesOf "Ну... Добре.") ["Ну...", "Добре."]
  , check "?! завершує речення" (sentencesOf "Що?! Так.") ["Що?!", "Так."]
  , check "символ … завершує речення" (sentencesOf "Далі… Кінець.") ["Далі…", "Кінець."]
  , check "десяткова крапка не ділить" (sentencesOf "Число 3.14 тут. Далі.") ["Число 3.14 тут.", "Далі."]
  , check "скорочення «напр.»" (sentencesOf "Є типи, напр. Int і Bool. Далі.") ["Є типи, напр. Int і Bool.", "Далі."]
  , check "«т. д.» і «т. п.»" (sentencesOf "Списки, кортежі і т. д. і т. п. Далі.") ["Списки, кортежі і т. д. і т. п.", "Далі."]
  , check "англійські скорочення" (sentencesOf "See Fig. 3 and Dr. Smith. Next.") ["See Fig. 3 and Dr. Smith.", "Next."]
  , check "ініціал" (sentencesOf "Писав І. Петренко. Далі.") ["Писав І. Петренко.", "Далі."]
  , check "закривальна лапка залишається в реченні" (sentencesOf "Він сказав: «Так!» Потім пішов.") ["Він сказав: «Так!»", "Потім пішов."]
  , check "крапка всередині ідентифікатора" (sentencesOf "Модуль Data.List містить. Так.") ["Модуль Data.List містить.", "Так."]
  , check "без кінцевого знака" (sentencesOf "Перше. Друге без крапки") ["Перше.", "Друге без крапки"]
  , check "порожній текст" (sentencesOf "") []
  , check "лише пробіли" (sentencesOf " \t\n ") []
  , check "нормалізація під час розбору" (sentencesOf "Один \t  два.\n\nТри   чотири!") ["Один два.", "Три чотири!"]
  , check "render . parse = normalize"
      (renderDocument (parseText messy)) (normalizeWhitespace messy)
  ]
  where
    messy = "Мова \t Haskell.  Дві   фрази!\n\nТретя ... четверта?  Dr.  Smith  — 3.14;  (x)  «y»"

replaceTests :: [Test]
replaceTests =
  [ check "коротша заміна"
      (sentenceAfter 1 3 "я" "Один два три.") (Right "Один я я.")
  , check "довша заміна"
      (sentenceAfter 1 3 "ВЕЛИКЕ_СЛОВО" "Один два три.") (Right "Один ВЕЛИКЕ_СЛОВО ВЕЛИКЕ_СЛОВО.")
  , check "заміна тієї ж довжини"
      (sentenceAfter 1 4 "ХХХХ" "Один два три.") (Right "ХХХХ два три.")
  , check "порожня заміна: слово всередині"
      (sentenceAfter 1 3 "" "Один два чотири.") (Right "Один чотири.")
  , check "порожня заміна: слово на початку"
      (sentenceAfter 1 3 "" "Кіт їсть рибу.") (Right "їсть рибу.")
  , check "порожня заміна: слово в кінці перед крапкою"
      (sentenceAfter 1 3 "" "Це їсть кіт.") (Right "Це їсть.")
  , check "порожня заміна: слово перед комою"
      (sentenceAfter 1 3 "" "Це кіт, а то пес.") (Right "Це, а то.")
  , check "порожня заміна: підряд два слова"
      (sentenceAfter 1 3 "" "Є кіт і пес поруч.") (Right "Є і поруч.")
  , check "порожня заміна: усі слова"
      (sentenceAfter 1 2 "" "Це ми.") (Right ".")
  , check "знаки на місці (кома, дужки, лапки)"
      (sentenceAfter 1 3 "ЯЯ" "Це (кіт), «пес»! Так.") (Right "Це (ЯЯ), «ЯЯ»!")
  , check "заміна містить знаки"
      (sentenceAfter 1 3 "a, b" "Це кіт.") (Right "Це a, b.")
  , check "пробіли в заміні нормалізуються"
      (sentenceAfter 1 3 "a \t  b" "Це кіт.") (Right "Це a b.")
  , check "пробіли на краях заміни відкидаються"
      (sentenceAfter 1 3 "  a  " "Це кіт.") (Right "Це a.")
  , check "лише пробіли в заміні = порожня заміна"
      (sentenceAfter 1 3 " \t " "Це кіт зараз.") (Right "Це зараз.")
  , check "довжина рахує апостроф"
      (sentenceAfter 1 4 "X" "М'яч і кіт.") (Right "X і кіт.")
  , check "довжина числа 3.14 = 4"
      (sentenceAfter 1 4 "pi" "Число 3.14 тут.") (Right "Число pi тут.")
  , check "підставлене слово повторно не заміняється"
      (sentenceAfter 1 3 "кот" "Це кіт.") (Right "Це кот.")
  , check "кількість замін"
      (replacementCount <$> replaceInDocument 1 3 "x" (parseText "Один два три.")) (Right 2)
  , check "кількість замін = 0"
      (replacementCount <$> replaceInDocument 1 9 "x" (parseText "Один два три.")) (Right 0)
  , check "змінюється лише вибране речення"
      (replaceText 2 3 "ЯЯЯЯ" sample) (Right "Один два три. ЯЯЯЯ і ЯЯЯЯ! Чи є кіт?")
  , check "решта речень дослівно рівні"
      (fmap (\r -> let Document a = replacementDocument r in (a !! 0, a !! 2))
            (replaceInDocument 2 3 "x" (parseText sample)))
      (Right (ss !! 0, ss !! 2))
  , check "кількість речень не змінюється"
      (fmap (length . documentSentences . replacementDocument) (replaceInDocument 2 3 "" (parseText sample)))
      (Right 3)
  , check "латинське слово серед кириличних"
      (sentenceAfter 1 7 "ok" "Мова Haskell тут.") (Right "Мова ok тут.")
  , check "номер 0" (replaceText 0 3 "x" sample) (Left (InvalidSentenceIndex 0 3))
  , check "номер більший за кількість" (replaceText 4 3 "x" sample) (Left (InvalidSentenceIndex 4 3))
  , check "від'ємний номер" (replaceText (-1) 3 "x" sample) (Left (InvalidSentenceIndex (-1) 3))
  , check "довжина 0" (replaceText 1 0 "x" sample) (Left (InvalidWordLength 0))
  , check "від'ємна довжина" (replaceText 1 (-2) "x" sample) (Left (InvalidWordLength (-2)))
  , check "порожній текст" (replaceText 1 3 "x" "") (Left NoSentences)
  , check "повідомлення: номер"
      (describeError (InvalidSentenceIndex 7 3)) "речення з номером 7 не існує (у тексті речень: 3, допустимі номери: 1..3)"
  , check "повідомлення: довжина"
      (describeError (InvalidWordLength 0)) "довжина слова має бути додатним цілим числом, отримано 0"
  , check "повідомлення: порожній текст"
      (describeError NoSentences) "у тексті немає жодного речення"
  ]
  where
    ss = documentSentences (parseText sample)

cliTests :: [Test]
cliTests =
  [ check "усі опції"
      (parseArgs ["-s", "2", "-l", "3", "-r", "abc", "f.txt"])
      (Right (Run (Options (Just "f.txt") (Just 2) (Just 3) (Just "abc") False False)))
  , check "довгі опції з ="
      (parseArgs ["--sentence=2", "--length=3", "--replace=", "f.txt"])
      (Right (Run (Options (Just "f.txt") (Just 2) (Just 3) (Just "") False False)))
  , check "значення, що починається з «-»"
      (parseArgs ["-r", "-x-", "-s", "-1"])
      (Right (Run emptyOptions {optReplacement = Just "-x-", optSentence = Just (-1)}))
  , check "--list і --text-only"
      (parseArgs ["--list", "--text-only"]) (Right (Run emptyOptions {optList = True, optTextOnly = True}))
  , check "довідка" (parseArgs ["x", "--help"]) (Right ShowHelp)
  , check "не число" (parseArgs ["-s", "abc"])
      (Left "значення «abc» опції -s не є цілим числом")
  , check "немає значення" (parseArgs ["-l"]) (Left "опція -l потребує значення")
  , check "невідома опція" (parseArgs ["--foo"]) (Left "невідома опція «--foo»")
  , check "два файли" (parseArgs ["a", "b"]) (Left "можна вказати лише один вхідний файл")
  , check "без аргументів" (parseArgs []) (Right (Run emptyOptions))
  ]

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let groups =
        [ ("Нормалізація пробілів", normalizeTests)
        , ("Розбір тексту", parserTests)
        , ("Заміна слів", replaceTests)
        , ("Командний рядок", cliTests)
        ]
  mapM_ (runGroup failures) groups
  n <- readIORef failures
  unless (n == 0) $ do
    putStrLn ("ПРОВАЛЕНО тестів: " ++ show n)
    exitFailure
  putStrLn "Усі тести пройдено."
  where
    runGroup failures (title, tests) = do
      putStrLn ("== " ++ title)
      mapM_ (runOne failures) tests
    runOne failures (name, ok) = do
      putStrLn ((if ok then "  ok   " else "  FAIL ") ++ name)
      unless ok (modifyIORef failures (+ 1))
