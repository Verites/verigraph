module GlobalOptions
  ( GlobalOptions
  , globalOpts
  , morphismsConf
  , verbose
  , inputFile
  , useConstraints
  ) where

import           Abstract.Category.FinitaryCategory (MorphismType (..))
import           Abstract.Rewriting.DPO             (MorphismsConfig (..), NacSatisfaction (..))
import           Data.Monoid                        ((<>))
import           Options.Applicative

data GlobalOptions = GOpts
  { arbitraryMatches         :: MatchRestriction
  , injectiveNacSatisfaction :: NacSatisfaction
  , verbose                  :: Bool
  , inputFile                :: String
  , useConstraints           :: Bool
  }

-- | Flag indicating what restrictions are required or assumed of matches.
data MatchRestriction = MonoMatches | AnyMatches deriving (Eq, Show)

-- | Converts a match restriction to the corresponding MorphismType
matchRestrictionToMorphismType :: MatchRestriction -> MorphismType
matchRestrictionToMorphismType MonoMatches = Monomorphism
matchRestrictionToMorphismType AnyMatches  = GenericMorphism

mt :: GlobalOptions -> MorphismType
mt = matchRestrictionToMorphismType . arbitraryMatches

morphismsConf :: GlobalOptions -> MorphismsConfig
morphismsConf opts = MorphismsConfig (mt opts) (injectiveNacSatisfaction opts)


globalOpts :: Parser GlobalOptions
globalOpts = GOpts
  <$> flag MonoMatches AnyMatches
    ( long "all-matches"
    <> help "Set the matches for arbitrary morphisms")
  <*> flag MonomorphicNAC PartiallyMonomorphicNAC
    ( long "partial-injective-nacs"
    <> help ("Restrict the analysis of NAC satisfaction to partially injective " ++
            "morphisms between the NAC graph and the instance graph"))
  <*> flag False True
    ( long "verbose" <> short 'v')
  <*> strArgument
    ( metavar "INPUT_FILE"
    <> action "file"
    <> help "GGX file defining the graph grammar")
  <*> flag False True
    ( long "use-constraints"
    <> help "Activate use of Constraints [EXPERIMENTAL: Concurrent Rules Only]")
