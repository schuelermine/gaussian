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

i :: Gaussian
i = 0 :+ 1

magnitude :: Floating c => Gaussian -> c
magnitude (a :+ b) = sqrt $ (fromInteger a ^ two) + (fromInteger b ^ two)
  where
    two = 2 :: Integer

norm :: Gaussian -> Integer
norm (a :+ b) = (a ^ two) + (b ^ two)
  where
    two = 2 :: Integer

conjugate :: Gaussian -> Gaussian
conjugate (a :+ b) = (a :+ negate b)

rotate_right :: Gaussian -> Gaussian
rotate_right (a :+ b) = (b :+ negate a)

rotate_left :: Gaussian -> Gaussian
rotate_left (a :+ b) = (negate b :+ a)

flip_x :: Gaussian -> Gaussian
flip_x (a :+ b) = (negate a :+ b)

flip_y :: Gaussian -> Gaussian
flip_y = conjugate

swap_x_y :: Gaussian -> Gaussian
swap_x_y (a :+ b) = (b :+ a)

toComplex :: Num p => Gaussian -> Complex.Complex p
toComplex (a :+ b) = (fromInteger a Complex.:+ fromInteger b)