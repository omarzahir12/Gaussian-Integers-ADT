{- Assignment
 - Name: Mohammad Omar Zahir
 -}
module Assign where

macid :: String
macid = "zahirm1"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: Returns the "Real" or first part of a Gaussian 
                Integer
 -}

gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: Returns the "Imaginary" or second part of a Gaussian 
                Integer
 -}

gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y

{- -----------------------------------------------------------------
 - gaussConj
 - -----------------------------------------------------------------
 - Description: Returns the conjugate of a gaussian integer, which is
                the same gaussian integer, except the imag part 
                multiplied by -1
 -}

gaussConj :: GaussianInt -> GaussianInt
gaussConj g = z where
    x = gaussReal(g)
    y = gaussImag(g)
    z = (x,-y)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: Returns the sum of two gaussian integers in the form:
                (a0 + b0i) + (a1 + b1i) = (a0 + a1) + (b0+b1)i 
 -}

gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = z where
    x = gaussReal(g0) + gaussReal(g1)
    y = gaussImag(g0) + gaussImag(g1)
    z = (x,y)

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: Returns the product of two gaussian integers in the 
                form:
                (a0 + b0i) * (a1 + b1i) = ((a0a1 - b0b1) + (a0b1 + b0a1)i)
 -}

gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = z where
    a = gaussReal(g0) * gaussReal(g1)
    b = gaussImag(g0) * gaussImag(g1)
    c = gaussReal(g0) * gaussImag(g1)
    d = gaussImag(g0) * gaussReal(g1)
    z = (a - b, c + d)

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: Returns the norm of a gaussian integer, which is the
                sum of th real and imaginary parts of the product of 
                a gaussian integer and its conjugate in the form:
                (a+bi) * (a-bi) = (a^2 + b^2)
 -}

gaussNorm :: GaussianInt -> Integer
gaussNorm g = y
    where
        x = gaussMult (g) (gaussConj g)
        y = gaussReal(x) + gaussImag(x)

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: Returns the gaussian integer with the greatest norm
                from a list of gaussian integers by applying a 
                supporting auxillary function, maxGaussNormAux, and
                returns a gaussian integer of (0,0) if input list empty
   ---------------------------------------------------------------
 - maxGaussNormAux
 - -----------------------------------------------------------------
 - Description: Returns the gaussian integer with the greatest norm
                from a list of gaussian integers by applying a 
                maxInList algorithm, except comparing the norms of 
                the gaussian integers
                
                Basic Procedure:
                 -Takes head and tail of the gaussian integer list as
                  input
                 -Assigns values to the input head and tail head for 
                  each repetition
                 -Sets a provisionary greatest normal for a gauss 
                  integer by comparing with head and discards or keeps
                  based on the greater value
                 -Continues this comparative process until the list 
                  ends
 -}
 
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm gs 
    | gs == [] = (0,0)
    | otherwise = maxGaussNormAux (tail gs) (head gs)

maxGaussNormAux :: [GaussianInt] -> GaussianInt -> GaussianInt
maxGaussNormAux m n
    | (m == []) = n
    | (y < x) = maxGaussNormAux (tail (m)) (n)
    | (y > x) = maxGaussNormAux (tail (m)) (head (m)) 
    | (y == x) = maxGaussNormAux (tail (m)) (n)
    where    
        x = gaussNorm (n)
        y = gaussNorm (head(m))
                                    

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussReal
 - - Test Case Number: 1
 - - Input: (3,-4)
 - - Expected Output: 3
 - - Acutal Output: 3
 - -----------------------------------------------------------------
 - - Function: gaussReal
 - - Test Case Number: 2
 - - Input: (-1,-9)
 - - Expected Output: -1
 - - Acutal Output: -1
 - -----------------------------------------------------------------
 - - Function: gaussReal
 - - Test Case Number: 3
 - - Input: (0,0)
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: gaussImag
 - - Test Case Number: 1 
 - - Input: (5,-9)
 - - Expected Output: -9
 - - Acutal Output: -9
 - -----------------------------------------------------------------
 - - Function: gaussImag
 - - Test Case Number: 2
 - - Input: (-3,2^2)
 - - Expected Output: 4
 - - Acutal Output: 4
 - -----------------------------------------------------------------
 - - Function: gaussImag
 - - Test Case Number: 3
 - - Input: (5,0)
 - - Expected Output:0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: (1,5)
 - - Expected Output: (1,-5)
 - - Acutal Output: (1,-5)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2
 - - Input: (-3,-8)
 - - Expected Output: (-3,8)
 - - Acutal Output: (-3,8)
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: (0,0)
 - - Expected Output: (0,0)
 - - Acutal Output:(0,0)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: (1,3) (4,-9)
 - - Expected Output: (5,-6)
 - - Acutal Output: (5,-6)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: (4,7) (0,0)
 - - Expected Output: (4,7)
 - - Acutal Output: (4,7)
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input:(-3,-4) (3,4)
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: (0,1) (3,5)
 - - Expected Output: (5,3)
 - - Acutal Output: (5,3)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: (-7,2) (4,6) 
 - - Expected Output: (-40,-34)
 - - Acutal Output: (-40,-34)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: (-5,0) (0,-8)
 - - Expected Output: (0,40)
 - - Acutal Output: (0,40)
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: (-5,3)
 - - Expected Output: 34
 - - Acutal Output: 34
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: (7,9)
 - - Expected Output: 130
 - - Acutal Output: 130
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: (0,0)
 - - Expected Output: 0
 - - Acutal Output: 0
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: []
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: [(5,9), (10,12), (3,9)]
 - - Expected Output: (10,12)
 - - Acutal Output: (10,12)
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: [(3,2), (2,3), (1,2)]
 - - Expected Output: (3,2)
 - - Acutal Output: (3,2)
 - -----------------------------------------------------------------
 -}

