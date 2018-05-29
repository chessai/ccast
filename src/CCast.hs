{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | This module provides type-safe conversions from Foreign C types
--   to Haskell types, and vice versa. It is especially convenient when
--   writing code with Haskell's C FFI.
--
--   This module will not compile on non-64-bit operating systems.
module CCast
  ( Cast(cast)
  , CComplex(CComplex)
  ) where

import Data.Coerce
import Data.Complex
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr 
import Foreign.Storable

import Bit64Only

-- | A typeclass for typesafe c-style casts from 'a' to 'b',
--   for use with Haskell's C FFI. This is just a convenience,
--   to prevent from having to think about numeric conversions.
--   Type inference is still weak here; I recommend two solutions:
--
--   With -XTypeApplications:
--
-- @
-- cast \@\CChar \@\Int8 x -- convert x from CChar to Int8
-- @
--
--   With explicit type annotations:
--
-- @
-- cast x :: Int8      -- convert x from CChar to Int8
-- @
--
class Cast a b where
  cast :: Coercible a b => a -> b
  cast = coerce;

-- | Complex number for FFI with the same memory layout as std::complex\<T\>
data CComplex a = CComplex !a !a
  deriving (Eq)

instance Storable a => Storable (CComplex a) where
    sizeOf _ = sizeOf (undefined :: a) * 2
    alignment _ = alignment (undefined :: a)
    poke p (CComplex x y) = do
        pokeElemOff (castPtr p) 0 x
        pokeElemOff (castPtr p) 1 y
    peek p = CComplex
        <$> peekElemOff (castPtr p) 0
        <*> peekElemOff (castPtr p) 1

instance Cast CChar   Int8    where
instance Cast Int8    CChar   where
instance Cast CSChar  Int8    where
instance Cast Int8    CSChar  where
instance Cast CUChar  Word8   where
instance Cast Word8   CUChar  where
instance Cast CShort  Int16   where
instance Cast Int16   CShort  where
instance Cast CUShort Word16  where
instance Cast Word16  CUShort where
instance Cast CInt    Int32   where
instance Cast Int32   CInt    where
instance Cast CUInt   Word32  where
instance Cast Word32  CUInt   where
instance Cast CInt    Int     where; cast = fromIntegral;
-- | WARNING! This conversion is potentially lossy.
instance Cast Int     CInt    where; cast = fromIntegral;
instance Cast CLong   Int64   where
instance Cast Int64   CLong   where
instance Cast CULong  Word64  where
instance Cast Word64  CULong  where
instance Cast CFloat  Float   where
instance Cast Float   CFloat  where
instance Cast CDouble Double  where
instance Cast Double  CDouble where

instance Cast (CComplex CFloat)  (Complex Float)    where; cast (CComplex x y) = cast x :+ cast y; {-# INLINE cast #-}
instance Cast (Complex Float)    (CComplex CFloat)  where; cast (x :+ y) = CComplex (cast x) (cast y); {-# INLINE cast #-} 
instance Cast (CComplex CDouble) (Complex Double)   where; cast (CComplex x y) = cast x :+ cast y; {-# INLINE cast #-}
instance Cast (Complex Double)   (CComplex CDouble) where; cast (x :+ y) = CComplex (cast x) (cast y); {-# INLINE cast #-}

instance Cast CPtrdiff   Int64      where
instance Cast Int64      CPtrdiff   where
instance Cast CSize      Word64     where
instance Cast Word64     CSize      where
instance Cast CWchar     Int32      where
instance Cast Int32      CWchar     where
instance Cast CSigAtomic Int32      where
instance Cast Int32      CSigAtomic where
instance Cast CLLong     Int64      where
instance Cast CULLong    Word64     where
instance Cast CBool      Word8      where
instance Cast Word8      CBool      where
instance Cast CIntPtr    Int64      where
instance Cast Int64      CIntPtr    where
instance Cast CUIntPtr   Word64     where
instance Cast Word64     CUIntPtr   where
instance Cast CIntMax    Int64      where
instance Cast Int64      CIntMax    where
instance Cast CClock     Int64      where
instance Cast Int64      CClock     where
instance Cast CTime      Int64      where
instance Cast Int64      CTime      where
instance Cast CUSeconds  Word32     where
instance Cast Word32     CUSeconds  where
instance Cast CSUSeconds Int64      where
instance Cast Int64      CSUSeconds where

-- Proof that we are on a 64-bit architecture.
-- This module will not compile outside of a 64-bit architecture.
myArchitecture :: Refined MustBe64 Int
myArchitecture = $$(refineTH (sizeOf (undefined :: Int)))
