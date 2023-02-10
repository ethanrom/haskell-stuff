--implementation of a linear quadratic regulator (LQR) control system in Haskell

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data

type State = Vector Double
type Control = Vector Double
type System = State -> Control -> State
type Cost = State -> Control -> Double

lqr :: System -> Cost -> State -> State -> Matrix Double
lqr sys cost xref uref = let
  n = dim xref
  m = dim uref
  q = quad cost xref uref
  a x u = sys x u
  b x u = jacobian sys x u
  ct = trans c
  c = (n><n) [2 * q x u | x <- toRows $ ident n, u <- toRows $ ident m]
  k = inv (b xref uref <> ct) <> ct <> a xref uref
  in k

main :: IO ()
main = do
  let xref = fromList [1, 2]
      uref = fromList [3, 4]
      sys x u = x + u
      cost x u = sumElements $ cmap (*2) (x - xref) + cmap (*2) (u - uref)
      k = lqr sys cost xref uref
  putStrLn $ "Gain matrix: " ++ show k
