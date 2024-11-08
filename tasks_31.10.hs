import Data.Array
newtype Matrix (Array (Int, Int) Double)
    deriving Show 
example :: [((Int, Int) Double)]    
example = [((0,0), 1), ((0, 1), 2)
           ((0, 2) 3), ((1, 0), 4)]
m1 = makeMatrix (3,3) example

-- FIRST TASK
makeMatrix :: (Int, Int) -> [((Int, Int), Double)] -> Matrix
makeMatrix (w, q) example = example ++ [(w, q), ((last example) + 1)]

(!!!) :: Matrix -> (Int, Int) -> Double
(!!!) example (a, s) = filter ((=a, =s)), _) example

matrixSize :: Matrix -> (Int, Int)
matrixSize example = last example

matrixIndex :: Matrix -> (Int, Int)
matrixIndex example = filter ((>=0, >=0), _) example


-- SECOND TASK
type MtxElem = ((Int, Int), Double)
matrixFold :: (b -> MtxElem -> b) -> b -> Matrix -> b
matrixFold example = foldl (+) example

matrixMap :: (MtxElem -> Double) -> Matrix -> Matrix
matrixMap (m1 e) example = map (*e) (map (++m1) example)

-- THIRD TASK
instance Show Matrix where
    show :: Matrix -> String
    show mtx = map fst example


-- FIFTH TASK
newtype Eqmtx :: (Eq Matrix) => Matrix -> Matrix -> Bool

-- SEVENTH TASK
det2 :: Matrix -> Double
det2 example = if (length example = 4)
                then length example
                else error
det3 :: Matrix -> Double
det3 example = if (length example = 9)
                then length example
                else error                



