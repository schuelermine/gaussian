{-# LANGAUGE NoImplicitPrelude #-}

module Gaussian where

import qualified Data.Complex as Complex
  -- Enables toComplex.
  -- Qualified to avoid clashing.

import Prelude hiding (quotRem)

import qualified Data.Ratio as Ratio

data Gaussian = (:+)
  {
    real :: !Integer,
    imaginary :: !Integer
  } deriving
    (Show, Read, Eq)
  -- The gaussian integer type.
  -- A gaussian integer is a complex number of the form a + bi where a and b are integers.

instance Num Gaussian
  where
    (a1 :+ b1) + (a2 :+ b2) = (a1 + a2) :+ (b1 + b2)
    (a1 :+ b1) - (a2 :+ b2) = (a1 - a2) :+ (b1 - b2)
    (a1 :+ b1) * (a2 :+ b2) = (a1 * a2 - b1 * b2) :+ (a1 * b2 + a2 * b1)
    abs (a :+ b) = ((a ^ two) + (b ^ two)) :+ 0
      where
        two = 2 :: Integer
          -- This prevents compiler warnings about defaulting types.
    signum = id
    negate (a :+ b) = negate a :+ negate b
    fromInteger x = x :+ 0
  -- The Num instance for Gaussian.
  -- We use the euclidean norm as abs: norm(a + bi) = a^2 + b^2

quotRem :: Gaussian -> Gaussian -> (Gaussian, Gaussian)
quotRem z1@(a1 :+ b1) z2@(a2 :+ b2) = (z3, z1 - z3 * z2)
  where
    z3 = a3 :+ b3
    a3 = floor $ (a1 * a2 + b1 * b2) % (a2 ^ two + b2 ^ two)
    b3 = floor $ (b1 * a2 - a1 * b2) % (a2 ^ two + b2 ^ two)
    two = 2 :: Integer

i :: Gaussian
i = 0 :+ 1
  -- The imaginary unit, equal to sqrt(-1).

magnitude :: Floating c => Gaussian -> c
magnitude (a :+ b) = sqrt $ (fromInteger a ^ two) + (fromInteger b ^ two)
  where
    two = 2 :: Integer
      -- This prevents compiler warnings about defaulting types.
  -- Equivalent to sqrt . fromInteger . real . abs

norm :: Gaussian -> Integer
norm (a :+ b) = (a ^ two) + (b ^ two)
  where
    two = 2 :: Integer
      -- This prevents compiler warnings about defaulting types.
  -- Equivalent to real . abs

conjugate :: Gaussian -> Gaussian
conjugate (a :+ b) = (a :+ negate b)
  -- The complex conjugate.
  -- Effectively flips about the real axis.

rotate_cw :: Gaussian -> Gaussian
rotate_cw (a :+ b) = (b :+ negate a)
  -- Rotate a gaussian integer clockwise.
  -- Equivalent to multiplying by i.

rotate_ccw :: Gaussian -> Gaussian
rotate_ccw (a :+ b) = (negate b :+ a)
  -- Rotate a gaussian integer counterclockwise.
  -- Equivalent to multiplying by -i.

flip_x :: Gaussian -> Gaussian
flip_x (a :+ b) = (negate a :+ b)
  -- Flips on imaginary axis.
  -- Analogous to conjugate.

flip_y :: Gaussian -> Gaussian
flip_y = conjugate
  -- Alias for conjugate.
  -- See conjugate.

swap_x_y :: Gaussian -> Gaussian
swap_x_y (a :+ b) = (b :+ a)
  -- Utility function, swaps real and imaginary axes.

flip_diag_2 :: Gaussian -> Gaussian
flip_diag_2 (a :+ b) = (negate b :+ negate a)
  -- Utility function, reflects on x = -y
  -- Equivalent to negate . swap_x_y

toComplex :: Num p => Gaussian -> Complex.Complex p
toComplex (a :+ b) = (fromInteger a Complex.:+ fromInteger b)
  -- Converts a Gaussian to any complex number: Num p => Complex p
  -- Fully qualified operators to avoid clashing.
