module Base.Valid
  ( ValidationResult
  , validate
  , isValid
  , errorMessages
  , ensureValid
  , Valid(..)
  , Validator
  , invalid
  , ensure
  , ensureM
  , withContext
  , validateNamed
  , mapValidator
  ) where

import Control.Monad.Writer.Lazy
import qualified Data.List   as List
import           System.Exit


-- | Result of validating some value.
data ValidationResult
  = IsValid -- ^ Indicates that no error was found.
  | IsInvalid [String] -- ^ Indicates that errors were found, with messages explaining them.

-- | Obtains the error messages produced by validation, or 'Nothing' if no error was found.
errorMessages :: ValidationResult -> Maybe String
errorMessages IsValid          = Nothing
errorMessages (IsInvalid msgs) = Just (List.intercalate "\n" msgs)

instance Monoid ValidationResult where

  mempty = IsValid

  mappend IsValid IsValid                     = IsValid
  mappend IsValid (IsInvalid msgs)            = IsInvalid msgs
  mappend (IsInvalid msgs) IsValid            = IsInvalid msgs
  mappend (IsInvalid msgs1) (IsInvalid msgs2) = IsInvalid (msgs1 ++ msgs2)


type Validator m = WriterT ValidationResult m ()

runValidator :: Monad m => Validator m -> m ValidationResult
runValidator = execWriterT

-- | Checks if the given value is well-formed, providing an explanation of any errors encountered.
validate :: Valid m a => a -> m ValidationResult
validate = runValidator . validator

-- | Checks if the given value is well-formed.
isValid :: Valid m a => a -> m Bool
isValid x = do
  result <- validate x
  return $ case result of
    IsValid -> True
    IsInvalid _ -> False

-- | Validates a list of named values, modifying their names with the given function.
validateNamed :: Valid m a => (name -> String) -> [(name, a)] -> m ValidationResult
validateNamed nameToContext items = runValidator (mapM_ validateItem items)
  where
    validateItem (name, item) =
      withContext (nameToContext name) (validator item)

-- | If the given validation detected some error, prints the error out and exits with failure.
ensureValid :: ValidationResult -> IO ()
ensureValid result =
  case errorMessages result of
    Nothing       -> return ()
    Just messages -> putStrLn messages >> exitFailure

-- | Type class for types that admit "malformed" values, and must thus be checked.
class Monad m => Valid m a where

  -- | Checks if the given value is well-formed, providing an explanation of any errors encountered.
  validator :: a -> Validator m

invalid :: Monad m => String -> Validator m
invalid message = tell (IsInvalid [message])

-- | Return 'IsValid' if the given boolean is true, otherwise return 'IsInvalid' with the given error message.
ensure :: Monad m => Bool -> String -> Validator m
ensure True _        = return ()
ensure False message = tell (IsInvalid [message])


-- | Return 'IsValid' if the given boolean is true, otherwise return 'IsInvalid' with the given error message.
ensureM :: Monad m => m Bool -> String -> Validator m
ensureM test message = do
  result <- lift test
  ensure result message

-- | Prepends the indication of context to any error messages produced by the validation result.
withContext :: Monad m => String -> Validator m -> Validator m
withContext context = censor prependContext
  where
    prependContext IsValid = IsValid
    prependContext (IsInvalid messages) = IsInvalid (map (prefix++) messages)
    prefix = context ++ ": "


-- | Changes the underlying monad of a validator.
--
-- It often happens that the validation of a particular type is done on a monad
-- @m@, but we need to validate it within a monad @n@. If there is a function
-- @convert :: m a -> m b@, then we can use @mapValidator convert validator@ to
-- change the underlying monad of the validation.
--
-- A common case is the use of monad transformers. Assume we have an instance
-- @Validate m a@, but we need to perform validation within the monad @t m@,
-- where @t@ is a 'MonadTransformer'. In this case, we can use @mapValidator
-- lift validator@.
mapValidator :: (m ((), ValidationResult) -> n ((), ValidationResult)) -> Validator m -> Validator n
mapValidator = mapWriterT