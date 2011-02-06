module Matrix where

import Foreign.C.Types
import Text.Printf

-- | Point2 stores a point in 2D space.
type Point2 = (CDouble, CDouble)

-- | Add an offset to a point.
addPt :: Point2 -> Point2 -> Point2
addPt (x, y) (x', y') = (x+x', y+y')

-- | Invert a point around the origin.
negPt :: Point2 -> Point2
negPt (x, y) = (-x, -y)

-- | Matrix3 stores what would be a 3*3 matrix, with the bottom row fixed as
-- the values (0, 0, 1). This allows the matrix to be used as a transformation
-- matrix in a homogeneous coordinate system, including stacking transforms by
-- multiplying the matrices. It does of course exclude the possibility
-- of perspective transforms, but since we're only in 2D that doesn't matter
-- anyway.

newtype Matrix3 = Matrix3 ( (CDouble, CDouble, CDouble)
                          , (CDouble, CDouble, CDouble)
                          ) deriving (Eq, Ord)
instance Show Matrix3 where
    show (Matrix3 ((a,b,c), (d,e,f))) =
        let [a',b',c',d',e',f'] = map realToFrac [a,b,c,d,e,f] :: [Double]
        in  printf "\n\t[[%6g, %6g, %6g]\n\t [%6g, %6g, %6g]]" a' b' c' d' e' f'

-- | Multiply two Matrix3 values.
(.*) :: Matrix3 -> Matrix3 -> Matrix3
(Matrix3 ((a,b,c), (d,e,f))) .* (Matrix3 ((k,l,m), (n,o,p))) =
    Matrix3 ( (a*k + b*n, a*l + b*o, a*m + b*p + c)
            , (d*k + e*n, d*l + e*o, d*m + e*p + f) )

-- | Multiply the matrix by a point. The point will be extended to a 3*1 matrix
-- by appending 1 as the last element (homogeneous coordinates).
(*.) :: Matrix3 -> Point2 -> Point2
(Matrix3 ((a,b,c), (d,e,f))) *. (x,y) = (a*x + b*y + c, d*x + e*y + f)

-- | The identity matrix.
idMat :: Matrix3
idMat = Matrix3 ((1, 0, 0), (0, 1, 0))

-- | A matrix that scales a point by a given factor in each dimension.
scaleMat :: CDouble -> CDouble -> Matrix3
scaleMat x y = Matrix3 ((x, 0, 0), (0, y, 0))

-- | A matrix that translates the point by the given amount.
translateMat :: Point2 -> Matrix3
translateMat (x, y) = Matrix3 ((1, 0, x), (0, 1, y))

-- | A matrix that rotates the point by the angle (in radians) around the origin.
rotateOriginMat :: CDouble -> Matrix3
rotateOriginMat ang =
    let c = cos ang
        s = sin ang
    in  Matrix3 ((c, -s, 0), (s, c, 0))

-- | A matrix that rotates around the given point.
rotateMat :: Point2 -> CDouble -> Matrix3
rotateMat pt ang =
    translateMat pt .* rotateOriginMat ang .* translateMat (negPt pt)


