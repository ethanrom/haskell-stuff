--Haskell implementation of the Kalman filter
import Numeric.LinearAlgebra

type State = Vector Double
type Covariance = Matrix Double
type Control = Vector Double
type Measurement = Vector Double

kalmanFilter :: State -> Covariance -> Control -> Measurement -> (State, Covariance)
kalmanFilter x P u z = let
  -- Prediction
  x' = x + u
  P' = P + identity (dim x)

  -- Update
  y = z - x'
  S = P' + identity (dim x)
  K = P' <\> trans S
  x'' = x' + K #> y
  P'' = P' - K <> trans K
  in (x'', P'')

main :: IO ()
main = do
  let x0 = fromList [0, 0]
      P0 = (2><2) [1, 0, 0, 1]
      u = fromList [1, 1]
      z = fromList [2, 2]
      (x, P) = kalmanFilter x0 P0 u z
  putStrLn $ "Estimated state: " ++ show x
  putStrLn $ "Estimated covariance: " ++ show P
