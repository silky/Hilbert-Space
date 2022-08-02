{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import           Control.Monad                       (replicateM)
import           Data.IORef                          (newIORef)
import           Data.List.Split                     (chunksOf)
import           Data.Random                         (runRVar)
import           Data.Random.Source.StdGen           (mkStdGen)
import           Diagrams.Prelude
import qualified Data.Random.Distribution.Uniform    as D

-- import           Diagrams.Backend.Rasterific.CmdLine 
import           Diagrams.Backend.SVG.CmdLine 

main :: IO ()
main = mainWith d

d :: IO (Diagram B)
d = do
  Right im <- loadImageEmb "hilbert-100.png"

  hs1 <- hilberts 0.02 1000 (image im # scale 0.002)

  return
      $ hs1 <> hs1 <> (square 1 # bg black)



hilberts :: Double -> Int -> Diagram B -> IO (Diagram B)
hilberts s count hilbert = do
  let dist  = D.uniform (-0.5) 0.5

  r <- newIORef (mkStdGen 1)
  [xs, ys] :: [[Double]] <- chunksOf count <$> ( flip runRVar r $ replicateM (count * 2) dist )

  let rx = 1
      ry = 1
  let pts = zipWith (\x y -> (rx * x) ^& (ry * y)) xs ys
      m   = position $ zip pts (repeat (hilbert # scale s))

  return m
