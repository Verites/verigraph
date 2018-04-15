{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString          as BS
import           Data.Monoid              ((<>))
import           Foreign.Lua              (Lua, runLua)
import qualified Foreign.Lua              as Lua
import           Options.Applicative
import qualified System.Console.Haskeline as Haskeline
import           System.FilePath          (takeDirectory)

import           GrLang                   (initialize)
import           Util.Lua

data Options = Options
  { repl        :: Bool
  , scriptToRun :: Maybe String
  }

shouldRunRepl :: Options -> Bool
shouldRunRepl (Options _ Nothing)     = True
shouldRunRepl (Options repl (Just _)) = repl

options :: Parser Options
options = Options
  <$> switch
    ( long "repl"
    <> help "Open a REPL even if given a script, in which case the script is run before entering the REPL.")
  <*> optional (strArgument
    ( metavar "FILE"
    <> help "Lua script to run before entering the REPL."))

fullOptions :: ParserInfo Options
fullOptions = info (options <**> helper)
  ( fullDesc
  <> progDesc "Lua interpreter with bindings to Verigraph." )

main :: IO ()
main = runLua $ do
  Lua.openlibs
  initialize

  options <- liftIO $ execParser fullOptions

  when (shouldRunRepl options) $ do
    liftIO $ putStrLn "Verigraph REPL"
    liftIO $ putStrLn ""

  case scriptToRun options of
    Just script -> do
      loaded <- checkStatus =<< Lua.loadfile script
      when loaded . addingToPath (takeDirectory script) . void $
        checkStatus =<< Lua.pcall 0 Lua.multret Nothing
    Nothing -> return ()

  when (shouldRunRepl options)
    readEvalPrintLoop
  where
    readEvalPrintLoop = do
      hasRead <- read'
      case hasRead of
        RError -> readEvalPrintLoop
        REnd -> return ()
        ROk -> do
          hasEvaled <- eval'
          when hasEvaled print'
          readEvalPrintLoop

    addingToPath dir action = do
      Lua.getglobal "package"
      Lua.getfield Lua.stackTop "path"
      oldPath <- Lua.tostring Lua.stackTop
      let newPath = stringToByteString (dir ++ "/?.lua;") <> oldPath
      Lua.pushstring newPath
      Lua.setfield (Lua.nthFromTop 3) "path"
      Lua.pop 2
      result <- action
      Lua.getglobal "package"
      Lua.pushstring oldPath
      Lua.setfield (Lua.nthFromTop 2) "path"
      return result

stringToByteString :: String -> BS.ByteString
stringToByteString = BS.pack . map (fromIntegral . fromEnum)

data ReaderResult = ROk | RError | REnd
getInputLine :: String -> Lua (Maybe String)
getInputLine = liftIO . Haskeline.runInputT settings . Haskeline.getInputLine
  where settings = Haskeline.defaultSettings { Haskeline.historyFile = Just ".verigraph-repl-history" }

read' :: Lua ReaderResult
read' = do
  input <- getInputLine "> "
  case input of
    Nothing -> return REnd
    Just line -> do
      status <- Lua.loadstring ("return " ++ line)
      case status of
        Lua.OK -> return ROk
        _ -> do
          Lua.pop 1
          readMultilineCommand line
  where
    readMultilineCommand content = do
      status <- Lua.loadstring content
      case status of
        Lua.OK -> return ROk
        Lua.ErrSyntax -> do
          isIncomplete <- testIfIncomplete
          if isIncomplete
            then do
              Lua.pop 1
              input <- getInputLine ">> "
              case input of
                Nothing -> return REnd
                Just line -> readMultilineCommand (content ++ '\n':line)
            else checkStatus status >> return RError
        _ -> checkStatus status >> return RError

    testIfIncomplete = ("<eof>" `ByteString.isSuffixOf`) <$> Lua.tostring Lua.stackTop

eval' :: Lua Bool
eval' = Lua.pcall 0 Lua.multret Nothing >>= checkStatus

print' :: Lua ()
print' = do
  stackTop <- Lua.gettop
  let numValues = fromIntegral (fromEnum stackTop)
  when (numValues > 0) $ do
    Lua.getglobal "print"
    Lua.insert Lua.stackBottom
    status <- Lua.pcall numValues 0 Nothing
    case status of
      Lua.OK -> return ()
      _ -> showError "Error calling print: "

