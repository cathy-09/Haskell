module Main where

import Text.Printf (printf)
goldenAngle :: Double
goldenAngle = pi * (3 - sqrt 5)
point :: Int -> (Double, Double)
point n = (sqrt (fromIntegral n), fromIntegral n * goldenAngle)
toXY :: (Double, Double) -> (Double, Double)
toXY (r, theta) = (r * cos theta, r * sin theta)
hslToHex :: Double -> Double -> Double -> String
hslToHex h s l = printf "#%02x%02x%02x" (to255 r) (to255 g) (to255 b)
  where
    c  = (1 - abs (2 * l - 1)) * s
    h' = h / 60
    x  = c * (1 - abs (snd (properFraction (h' / 2) :: (Int, Double)) * 2 - 1))
    m  = l - c / 2
    (r0, g0, b0)
      | h' < 1    = (c, x, 0)
      | h' < 2    = (x, c, 0)
      | h' < 3    = (0, c, x)
      | h' < 4    = (0, x, c)
      | h' < 5    = (x, 0, c)
      | otherwise = (c, 0, x)
    (r, g, b) = (r0 + m, g0 + m, b0 + m)
    to255 v = round (v * 255) :: Int
seedSvg :: Double -> Double -> Int -> Int -> String
seedSvg cx cy scale n =
  printf "  <circle cx=\"%.2f\" cy=\"%.2f\" r=\"%.2f\" fill=\"%s\" fill-opacity=\"0.85\"/>"
         (cx + x * fromIntegral scale')
         (cy - y * fromIntegral scale')
         radius
         color
  where
    (r, theta) = point n
    (x, y)     = toXY (r, theta)
    scale'     = scale
    radius     = 2.2 + 0.05 * r
    hue        = fromIntegral (n `mod` 360)
    color      = hslToHex hue 0.75 0.55
flowerSvg :: Int -> String
flowerSvg n = unlines $
  [ printf "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">" size size size size
  , printf "  <rect width=\"%d\" height=\"%d\" fill=\"#0b0b16\"/>" size size
  ] ++
  [ seedSvg cx cy scale i | i <- [1 .. n] ] ++
  [ "</svg>" ]
  where
    size  = 800 :: Int
    cx    = fromIntegral size / 2
    cy    = fromIntegral size / 2
    scale = 18 :: Int

main :: IO ()
main = do
  let svg = flowerSvg 600
  writeFile "flower.svg" svg
  putStrLn "Ready -> flower.svg"
