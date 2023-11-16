module Main where
import Yggdrasil.Extensions.ParseExisting
import Yggdrasil.Config
main :: IO ()
main = do
  c <- readMyConfig
  case c of
    Right mc ->
      testParsing mc
    Left e -> fail e
