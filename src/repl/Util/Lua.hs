module Util.Lua where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString     as ByteString
import           Foreign.Lua
import qualified Foreign.Lua         as Lua

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

luaError :: String -> Lua NumResults
luaError msg = do
  Lua.push "_HASKELLERR"
  Lua.push msg
  return 2

execLua :: String -> Lua ()
execLua code = do
  hasRead <- checkStatus =<< Lua.loadstring code
  when hasRead $ do
    checkStatus =<< Lua.pcall 0 0 Nothing
    return ()
