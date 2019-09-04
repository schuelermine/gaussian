module Gaussian where

import qualified Data.Complex as Complex

data Gaussian = (:+)
  {
    real :: Integer,
    imaginary :: Integer
  } deriving
    (Show, Read, Eq)

instance Num Gaussian
  where
    (a1 :+ b1) + (a2 :+ b2) = (a1 + a2) :+ (b1 + b2)
    (a1 :+ b1) - (a2 :+ b2) = (a1 - a2) :+ (b1 - b2)
    (a1 :+ b1) * (a2 :+ b2) = (a1 * a2 - b1 * b2) :+ (a1 * b2 + a2 * b1)
    abs (a :+ b) = ((a ^ two) + (b ^ two)) :+ 0
      where
        two = 2 :: Integer
    signum = id
    negate (a :+ b) = negate a :+ negate b
    fromInteger x = x :+ 0

conjugate :: Gaussian -> Gaussian
conjugate (a :+ b) = (a :+ negate b)

i :: Gaussian
i = 0 :+ 1

magnitude :: Floating c => Gaussian -> c
magnitude (a :+ b) = sqrt $ (fromInteger a ^ two) + (fromInteger b ^ two)
  where
    two = 2 :: Integer

rotate_right :: Gaussian -> Gaussian
rotate_right = (* (-i))

rotate_left :: Gaussian -> Gaussian
rotate_left = (* i)

toComplex :: Num p => Gaussian -> Complex.Complex p
toComplex (a :+ b) = (fromInteger a Complex.:+ fromInteger b)