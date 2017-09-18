module GlobalOptions
  ( GlobalOptions(..)
  , MatchRestriction(..)
  , globalOpts
  , morphismsConf
  ) where

import           Data.Monoid            ((<>))
import           Options.Applicative

import           Abstract.Category
import           Abstract.Rewriting.DPO (MorphismsConfig (..))
import           Category.TypedGraph

data GlobalOptions = GOpts
  { arbitraryMatches :: MatchRestriction
  , verbose          :: Bool
  , inputFile        :: String
  , useConstraints   :: Bool
  }

-- | Flag indicating what restrictions are required or assumed of matches.
data MatchRestriction = MonoMatches | AnyMatches deriving (Eq, Show)

-- | Converts a match restriction to the corresponding MorphismType
matchRestrictionToMorphismType :: MatchRestriction -> MorphismClass (TypedGraphMorphism a b)
matchRestrictionToMorphismType MonoMatches = monic
matchRestrictionToMorphismType AnyMatches  = anyMorphism

morphismsConf :: GlobalOptions -> MorphismsConfig (TypedGraphMorphism a b)
morphismsConf = MorphismsConfig . matchRestrictionToMorphismType . arbitraryMatches


globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag MonoMatches AnyMatches
    ( long "all-matches"
    <> help "Set the matches for arbitrary morphisms")
  <*> flag False True
    ( long "verbose" <> short 'v')
  <*> strArgument
    ( metavar "INPUT_FILE"
    <> action "file"
    <> help "GGX file defining the graph grammar")
  <*> flag False True
    ( long "use-constraints"
    <> help "Activate use of Constraints [EXPERIMENTAL: Concurrent Rules Only]")
