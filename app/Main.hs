{-# LANGUAGE QuasiQuotes #-}
module Main where

import Lib
import Data.String.Interpolate (i)

main :: IO ()
main = do
  let a = 1
  let b = 2
  putStrLn [i| Sum of #{a} and #{b} is #{sum' a b}|]
