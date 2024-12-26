module Main where


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


origin :: Point
origin = makeCurve [] 0.0

distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = 
    let dx = x1 Prelude.- x2
        dy = y1 Prelude.- y2 
        rx = dx Prelude.* rx
        ry = dy Prelude.* dy
    in sqrt (rx Prelude.+ ry)

arcLength :: VectorGraphic -> Point -> Point -> Float -> Float
arcLength v start end precision  
    | distance start end <= precision = distance start end
    | otherwise = arcLength v start mid precision Prelude.+ arcLength v mid end precision 
        where mid = 0.5 Main.* (start Main.+ end)

withinTolerance :: Float -> Float -> Float -> Bool
withinTolerance x goal tol 
    | goal Prelude.- tol <= x && x <= goal Prelude.+ tol = True
    | otherwise = False 


    

parameterizeByArcLength :: VectorGraphic -> Float -> VectorGraphic
parameterizeByArcLength v precision s = parameterizeByArcLength' v precision start end s
    where parameterizeByArcLength' :: VectorGraphic -> Float -> Point -> Point -> VectorGraphic
          parameterizeByArcLength' v p a b s = 
            let arc = arcLength v origin b precision
                mid = 0.5 Main.* (b Main.- a)
                in case () of _
                                | withinTolerance arc s precision -> mid
                                | arc < s -> parameterizeByArcLength' v p mid b s 
                                | arc > s -> parameterizeByArcLength' v p a mid s
          start = origin
          end = v 1.0




main :: IO()
main = putStrLn "Hello World"
