module Base.Cardinality (

  Cardinality(..)

)
where

class Cardinality o where

  -- | Given an object, returns its cardinality
  cardinality :: o -> Int
