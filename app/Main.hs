module Main (main) where

import Control.Exception (IOException, try)
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import Text.Read (readMaybe)

import Textbook.CLI
import Textbook.Parser (parseText)
import Textbook.Render
import Textbook.Replace
import Textbook.Types

main :: IO ()
main = do
  -- UTF-8 незалежно від локалі (аргументи, файли, stdin/stdout)
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  args <- getArgs
  case parseArgs args of
    Left err -> failWith (err ++ "\n\n" ++ usageText)
    Right ShowHelp -> putStr usageText
    Right (Run opts) -> run opts

run :: Options -> IO ()
run opts = do
  path <- maybe (prompt "Шлях до файлу з текстом") pure (optFile opts)
  doc <- readDocument path
  let total = length (documentSentences doc)
  if optList opts
    then putStr (renderNumbered doc)
    else do
      idx <- maybe (promptInt ("Номер речення (1.." ++ show total ++ ")")) pure (optSentence opts)
      len <- maybe (promptInt "Довжина слів для заміни") pure (optLength opts)
      sub <- maybe (prompt "Підрядок-заміна (може бути порожнім)") pure (optReplacement opts)
      case replaceInDocument idx len sub doc of
        Left err -> failWith ("Помилка: " ++ describeError err)
        Right r -> report opts len r

report :: Options -> Int -> Replacement -> IO ()
report opts len r
  | optTextOnly opts = putStrLn (renderDocument (replacementDocument r))
  | otherwise = do
      putStrLn ("Було:  " ++ renderSentence (replacementOriginal r))
      putStrLn ("Стало: " ++ renderSentence (replacementSentence r))
      putStrLn
        ( if replacementCount r == 0
            then "Слів довжини " ++ show len ++ " у реченні немає, текст не змінено."
            else "Замінено слів: " ++ show (replacementCount r)
        )
      putStrLn ""
      putStrLn "Весь текст:"
      putStrLn (renderDocument (replacementDocument r))

readDocument :: FilePath -> IO Document
readDocument path = do
  result <- try (withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    hGetContents' h) :: IO (Either IOException String)
  case result of
    Left e -> failWith ("Помилка: не вдалося прочитати файл «" ++ path ++ "»: " ++ show e)
    Right content -> pure (parseText (dropBom content))
  where
    dropBom ('\xFEFF' : s) = s
    dropBom s = s

prompt :: String -> IO String
prompt label = do
  putStr (label ++ ": ")
  hFlush stdout
  eof <- isEOF
  if eof
    then failWith "\nПомилка: вхідні дані закінчилися, а параметр не задано"
    else getLine

promptInt :: String -> IO Int
promptInt label = do
  s <- prompt label
  case readMaybe s of
    Just n -> pure n
    Nothing -> failWith ("Помилка: «" ++ s ++ "» не є цілим числом")

failWith :: String -> IO a
failWith msg = hPutStrLn stderr msg >> exitWith (ExitFailure 1)
