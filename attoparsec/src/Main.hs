{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as ByteString
import           Data.Text                        (Text)
import qualified Data.Text                        as Text (pack)

data JSFunction = JSFunction { jsfName :: Text
                             , jsfBody :: Text
                             }
  deriving(Show)

parseJSFunction :: Parser JSFunction
parseJSFunction = do
    _ <- string "function"
    space
    name <- manyTill anyChar (string "(")
    -- Arguments
    _ <- manyTill anyChar (string ")")
    space
    string "{"
    endOfLine
    body <- manyTill anyChar (string "}")
    return (JSFunction (Text.pack name) (Text.pack body))

main :: IO ()
main = do
    input <- ByteString.readFile "something.js"
    let result = parse parseJSFunction input
    print result
    ByteString.putStrLn input
