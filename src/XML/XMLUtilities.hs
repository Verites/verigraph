{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.XMLUtilities
( atTag
, text
, textAtTag
, parseXML
)

where

import           Data.Char
import           Text.XML.HXT.Core

atTag tag = deep (isElem >>> hasName tag)

text = getChildren >>> getText

textAtTag tag = atTag tag >>> text

parseXML = readDocument [withValidate no, withRemoveWS yes]
