{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Foreign.Lua (Lua, runLua)
import qualified Foreign.Lua as Lua
import qualified Data.ByteString as ByteString
import qualified System.Console.Haskeline as Haskeline

main :: IO ()
main = runLua $ do
  Lua.openlibs

  liftIO $ putStrLn "Verigraph REPL"
  liftIO $ putStrLn ""

  loop
  where
    loop = do
      hasRead <- read'
      case hasRead of
        RError -> loop
        REnd -> return ()
        ROk -> do
          hasEvaled <- eval'
          when hasEvaled print'
          loop

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
    
    testIfIncomplete = ("<eof>" `ByteString.isSuffixOf`) <$> Lua.tostring (-1)

eval' :: Lua Bool
eval' = Lua.pcall 0 Lua.multret Nothing >>= checkStatus

print' :: Lua ()
print' = do
  stackTop <- Lua.gettop
  let numValues = fromIntegral (fromEnum stackTop)
  when (numValues > 0) $ do
    Lua.getglobal "print"
    Lua.insert 1
    status <- Lua.pcall numValues 0 Nothing
    case status of
      Lua.OK -> return ()
      _ -> showError "Error calling print: "

checkStatus :: Lua.Status -> Lua Bool
checkStatus Lua.OK = return True
checkStatus status = showError prefix >> return False
  where prefix = case status of
          Lua.ErrSyntax -> "Syntax error: "
          Lua.ErrMem -> "Out of memory error: "
          Lua.ErrRun -> "Error: "
          _ -> "Critical error: "

showError :: String -> Lua ()
showError prefix = do
  msg <- Lua.tostring (-1)
  liftIO $ putStr prefix >> ByteString.putStr msg >> putStrLn ""
  Lua.pop 1
