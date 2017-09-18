{-# LANGUAGE NoMonomorphismRestriction #-}

module XML.XMLUtilities
( atTag
, text
, textAtTag
, parseXML
, writeXML
)

where

import           Data.Tree.NTree.TypeDefs
import           Text.XML.HXT.Core

atTag :: ArrowXml cat => String -> cat (NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml cat => cat (NTree XNode) String
text = getChildren >>> getText

textAtTag :: ArrowXml cat => String -> cat (Data.Tree.NTree.TypeDefs.NTree XNode) String
textAtTag tag = atTag tag >>> text

parseXML :: String -> IOStateArrow s b XmlTree
parseXML = readDocument [withValidate no, withRemoveWS yes]


writeXML :: IOStateArrow () XmlTree XmlTree -> String -> IO [XmlTree]
writeXML doc file = runX $ root [] [doc] >>> writeDocument [withIndent yes] file