module Main where
import Distribution.SPDX (LicenseId(MakeIndex))
import Text.PrettyPrint.Annotated.HughesPJClass (PrettyLevel(PrettyLevel))


class Vector a where 
    (+) :: a -> a -> a 
    (-) :: a -> a -> a
    (*) :: Float -> a -> a
    negate :: a -> a

type Point = (Float, Float)  
addPoint :: Point -> Point -> Point
addPoint (x, y) (r, s) = (x Prelude.+ r, y Prelude.+ s)

subtractPoint :: Point -> Point -> Point
subtractPoint (x, y) (r, s) = (x Prelude.- r, y Prelude.- s)

scalePoint :: Float -> Point -> Point 
scalePoint s (x, y) = (s Prelude.* x, s Prelude.* y)

negatePoint :: Point -> Point 
negatePoint (x, y) = (Prelude.negate x, Prelude.negate y)

instance Vector Point where
    (+) = addPoint
    (-) = subtractPoint
    (*) = scalePoint
    negate = negatePoint


type VectorGraphic = Float -> (Float, Float)
vectorGraphicParameter :: Float -> Float
vectorGraphicParameter x 
    | x >= 0 = x Prelude.- fromIntegral (floor x) :: Float
    | otherwise = vectorGraphicParameter (Prelude.negate x)

addVectorGraphic :: VectorGraphic -> VectorGraphic -> VectorGraphic
addVectorGraphic v w t = case () of 
    _ | vectorGraphicParameter t <= 0.5 -> v t
      | otherwise -> w t

negateVectorGraphic :: VectorGraphic -> VectorGraphic 
negateVectorGraphic v = v . Prelude.negate  

subtractVectorGraphic :: VectorGraphic -> VectorGraphic -> VectorGraphic
subtractVectorGraphic v w = addVectorGraphic v (negateVectorGraphic w)

scaleVectorGraphic :: Float -> VectorGraphic -> VectorGraphic
scaleVectorGraphic s v t = (s Prelude.* x, s Prelude.* y) where (x, y) = v t

instance Vector VectorGraphic where
    (+) = addVectorGraphic
    (-) = subtractVectorGraphic
    (*) = scaleVectorGraphic
    negate = negateVectorGraphic




makeCurve :: [Main.Point] -> VectorGraphic
makeCurve [] t = (0.0, 0.0) :: Main.Point
makeCurve [p] t = p
makeCurve [start, end] t = start Main.+ (t Main.* (end Main.- start)) 
makeCurve [start, mid, end] t = 
    (a Main.* start) Main.+ (b Main.* mid) Main.+ (c Main.* end) 
    where 
        a = (1 Prelude.- t) Prelude.* (1 Prelude.- t)
        b = 2 Prelude.* (1 Prelude.- t) Prelude.* t
        c = t Prelude.* t




main :: IO()
main = putStrLn "Hello World"
