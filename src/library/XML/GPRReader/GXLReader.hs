module XML.GPRReader.GXLReader (readGrammar, readGGName) where

import qualified Data.List                      as L
import           System.Directory
import           System.FilePath
import           System.IO

import           Abstract.Rewriting.DPO
import           Base.Valid
import           Data.TypedGraph.Morphism       (domainGraph, TypedGraphMorphism)
import           XML.GPRReader.GXLInstatiator
import           XML.GPRReader.GXLParseIn
import           XML.GPRReader.GXLPreProcessing

type Names = [(String,String)]

readGrammar :: String -> IO (Grammar (TypedGraphMorphism a b), Names)
readGrammar fileName = do
  files <- getDirectoryContents fileName
  
  -- system.properties file
  let systemPropertiesFile = "system.properties" --head (filter (\name -> takeExtension name == ".properties") files)
      systemPropertiesPath = fileName ++ "/" ++ systemPropertiesFile
  
  handleProperties <- openFile systemPropertiesPath ReadMode
  systemProperties <- hGetContents handleProperties
  
  let typeGraphName = getOption systemProperties "typeGraph="
      stateGraphName = getOption systemProperties "startGraph="
  
  -- type graph
  let typeGraphPathName = fileName ++ "/" ++ typeGraphName ++ ".gty"
  
  parsedTypeGraph <- parseGPR typeGraphPathName
  
  let typesWithId = processTypeGraph parsedTypeGraph      
      typeGraph = instatiateTypeGraph parsedTypeGraph
  
  -- initial state, it uses the rule parser to get the initial state
  let stateGraphPathName = fileName ++ "/" ++ stateGraphName ++ ".gst"
  stateGraph <- parseGPR stateGraphPathName
  let (_,stateRule) = instatiateOneRule typeGraph typesWithId stateGraph
      initialState = domainGraph (getLHS stateRule)
  
  -- rules
  let ruleNames = filter (\name -> takeExtension name == ".gpr") files
      rulePathNames = map ((fileName ++ "/") ++) ruleNames
  parsedRules <- mapM parseGPR rulePathNames
  
  let rules = instatiateRules typeGraph typesWithId parsedRules
  
  ensureValid $ validateNamed (\name -> "Rule '"++name++"'") rules
  _ <- (L.null rules && error "No first order productions were found, at least one is needed.") `seq` return ()
  
  let instatiatedGrammar = grammar initialState [] rules
      preparedNames = prepareNames typesWithId
  
  return (instatiatedGrammar, preparedNames)

readGGName :: String -> String
readGGName = removeExtension. removePath . removeLastSlash
  where
    removeExtension = takeWhile (/= '.')
    removePath str = reverse (takeWhile (/= '/') (reverse str))
    removeLastSlash str = if last str == '/' then init str else str

prepareNames :: ProcessedTypeGraph -> Names
prepareNames (nodes,edges) = preparedNodes ++ preparedEdges
  where
    preparedNodes = map (\(lbl,id) -> ("I" ++ show id, lbl ++ "%:RECT:java.awt.Color[r=0,g=0,b=0]:[NODE]:")) nodes
    preparedEdges = map (\(src,tgt,lbl,id) -> ("I" ++ show id, lbl ++ "(" ++ show src ++ "_" ++ show tgt ++ ")%:SOLID_LINE:java.awt.Color[r=0,g=0,b=0]:[EDGE]:")) edges

-- Auxiliary function to read a text file with grammar properties 
getOption :: String -> String -> String
getOption systemProperties string = tail (L.dropWhile (/= '=') unique)
  where
    inLines = lines systemProperties
    names = filter (L.isPrefixOf string) inLines
    unique = case names of
               []    -> error ("error, '" ++ string ++ "' not found")
               ptg:_ -> ptg
