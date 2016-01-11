{-# LANGUAGE TemplateHaskell #-}
module Main where

import           TH

data MyType = MyType

deriveShowType ''MyType

main :: IO ()
main = print (myShow MyType)
