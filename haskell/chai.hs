
import System.Environment (getArgs)

import Data.Char (isDigit)

import Data.List (nub)



lerp s e r = s + r * (e - s)

lp (sx, sy) (ex, ey) r = (lerp sx ex r, lerp sy ey r)

cut r p p' = let r' = if r > 0.5 then 1 - r else r in [lp p p' r', lp p' p r']

chaikin r xs =
  concat $ zipWith const (zipWith (cut r) (cycle xs) (tail $ cycle xs)) xs

escape ((x, y), c) = "\x1b["

                  ++ show y ++ ";"

                  ++ show (2*x)

                  ++ "H\x1b[48;5;"

                  ++ show c ++ "m  "

draw shape resolution =

  let iterations = iterate (chaikin 0.35) shape

      colours    = [1..]

      rounded    = map (nub . map (\(x, y) -> (round x, round y))) iterations

      points     = zipWith (\x -> zip x . repeat) rounded colours

      string     = concatMap (concatMap escape) (take resolution points)

  in putStr string



main = do

  args <- getArgs

  print args

  case args of

    [num, shape] -> draw (read shape :: [(Double, Double)]) (read num :: Int)

    _ -> putStr "I don't know what you mean"

  putStr "\x1b[0m"
