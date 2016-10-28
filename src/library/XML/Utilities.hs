module XML.Utilities where

import           Data.Maybe (fromMaybe)
import           Text.Read  (readMaybe)

fstOfThree :: (t1, t2, t3) -> t1
fstOfThree (x,_,_) = x

-- | Reads the id from the last to the head, stops when do not found a number
clearIdUnsafe :: String -> String
clearIdUnsafe [] = error "Error reading id"
clearIdUnsafe l = if isNum (last l) then clearId (init l) ++ [last l] else ""
  where
   isNum x = x `elem` "0123456789"

-- | Reads the id from the last to the head
clearId :: String -> String
clearId [] = ""
clearId l = if isNum (last l) then clearId (init l) ++ [last l] else ""
  where
   isNum x = x `elem` "0123456789"

toN :: String -> Int
toN x = fromMaybe (error $ "Error converting id (" ++ x ++ ") to number") (readMaybe x :: Maybe Int)
