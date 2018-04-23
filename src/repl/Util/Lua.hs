module Util.Lua where

import           Control.Monad
import           Control.Monad.Trans
import qualified Data.ByteString     as ByteString
import           Foreign.Lua
import qualified Foreign.Lua         as Lua
import           System.IO           (hPutStr, hPutStrLn, stderr)

checkStatus :: Lua.Status -> Lua Bool
checkStatus Lua.OK = return True
checkStatus status = showError prefix >> return False
  where prefix = case status of
          Lua.ErrSyntax -> "Syntax error: "
          Lua.ErrMem -> "Out of memory error: "
          Lua.ErrRun -> "Error: "
          _ -> "Critical error: "

pushFunction :: ToHaskellFunction a => a -> Lua ()
pushFunction f =
  Lua.pushHaskellFunction f >> Lua.wrapHaskellFunction

showError :: String -> Lua ()
showError prefix = do
  msg <- Lua.tostring Lua.stackTop
  liftIO $ do
    hPutStr stderr prefix
    ByteString.hPutStr stderr msg
    hPutStrLn stderr ""
  Lua.pop 1

luaError :: String -> Lua NumResults
luaError msg = do
  Lua.push msg
  fromIntegral <$> Lua.lerror

execLuaFile :: FilePath -> Lua ()
execLuaFile path = do
  hasRead <- checkStatus =<< Lua.loadfile path
  when hasRead $ do
    checkStatus =<< Lua.pcall 0 0 Nothing
    return ()

execLua :: String -> Lua ()
execLua code = do
  hasRead <- checkStatus =<< Lua.loadstring code
  when hasRead $ do
    checkStatus =<< Lua.pcall 0 0 Nothing
    return ()
