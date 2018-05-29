{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_HADDOCK hide #-}

-- | Make something fail to compile on certain architectures.
module Bit64Only
  ( refine
  , refineTH
  , Refined(..)
  , Architecture(..)
  , intToArchitecture
  , Predicate(..)
  , MustBe64
  ) where

import Data.Function

import qualified Language.Haskell.TH.Syntax as TH

refine :: Predicate p x => x -> Either String (Refined p x)
refine x =
  fix $ \result ->
    maybe (Right (Refined x)) Left $
    validate (predicateByResult result) x
  where
    -- A work-around for the type-inference.
    predicateByResult :: Either String (Refined p x) -> p
    predicateByResult =
      const undefined

refineTH :: (Predicate p x, TH.Lift x) => x -> TH.Q (TH.TExp (Refined p x))
refineTH =
  fix $ \loop ->
    fmap TH.TExp . either fail TH.lift . refineByResult (loop undefined)
  where
    -- A work-around for the type-inference.
    refineByResult :: Predicate p x => TH.Q (TH.TExp (Refined p x)) -> x -> Either String (Refined p x)
    refineByResult =
      const refine

newtype Refined p x = Refined x

instance TH.Lift x => TH.Lift (Refined p x) where
  lift (Refined a) = [|Refined a|]

data Architecture = Bit32 | Bit64 | Unknown

intToArchitecture :: Int -> Architecture
intToArchitecture 4 = Bit32
intToArchitecture 8 = Bit64
intToArchitecture _ = Unknown

class Predicate p x where
  validate :: p -> x -> Maybe String

data MustBe64

instance (x ~ Int) => Predicate MustBe64 x where
  validate _ x =
    case intToArchitecture x of
      Bit64 -> Nothing
      _     -> Just "Your architecture is not 64-bit. Failing to compile."
