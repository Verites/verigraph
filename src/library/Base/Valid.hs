module Base.Valid
  ( Valid(..)
  , ValidationResult(..)
  , ensure
  , withContext
  , errorMessages
  , validateNamed
  , ensureValid
  ) where

import qualified Data.List   as List
import           System.Exit


-- | Type class for types that admit "malformed" values, and must thus be checked.
--
-- Although implementing the 'isValid' method is sufficient, it will result in poor
-- error messages. Therefore, implementing the 'validate' method is recommended.
class Valid a where

    -- | Checks if the given value is well-formed
    isValid :: a -> Bool
    isValid x =
      case validate x of
        IsValid     -> True
        IsInvalid _ -> False

    -- | Checks if the given value is well-formed, providing an explanation of any errors encountered.
    validate :: a -> ValidationResult
    validate x =
      if isValid x then
        IsValid
      else
        IsInvalid ["<invalid value>"]

    {-# MINIMAL isValid | validate #-}



-- | Result of validating some value
data ValidationResult
  = IsValid -- ^ Indicates that no error was found
  | IsInvalid [String] -- ^ Indicates that errors were found, with messages explaining them
  deriving (Eq, Show)


instance Monoid ValidationResult where

  mempty = IsValid

  mappend IsValid IsValid                     = IsValid
  mappend IsValid (IsInvalid msgs)            = IsInvalid msgs
  mappend (IsInvalid msgs) IsValid            = IsInvalid msgs
  mappend (IsInvalid msgs1) (IsInvalid msgs2) = IsInvalid (msgs1 ++ msgs2)


-- | Return 'IsValid' if the given boolean is true, otherwise return 'IsInvalid' with the given error message.
ensure :: Bool -> String -> ValidationResult
ensure True _        = IsValid
ensure False message = IsInvalid [message]


-- | Prepends the indication of context to any error messages produced by the validation result.
withContext :: String -> ValidationResult -> ValidationResult
withContext _ IsValid =
  IsValid


withContext context (IsInvalid messages) =
  IsInvalid (map prependContext messages)
  where
    prependContext msg = context ++ ": " ++ msg


-- | Obtains the error messages produced by validation, or 'Nothing' if no error was found
errorMessages :: ValidationResult -> Maybe String
errorMessages IsValid          = Nothing
errorMessages (IsInvalid msgs) = Just (List.intercalate "\n" msgs)


-- | Validates a list of named values, modifying their names with the given function.
validateNamed :: Valid a => (name -> String) -> [(name, a)] -> ValidationResult
validateNamed nameToContext items =
  mconcat (map validateItem items)
  where
    validateItem (name, item) =
      withContext (nameToContext name) (validate item)

-- | If the given validation detected some error, prints the error out and exits with failure.
ensureValid :: ValidationResult -> IO ()
ensureValid result =
  case errorMessages result of
    Nothing       -> return ()
    Just messages -> putStrLn messages >> exitFailure
