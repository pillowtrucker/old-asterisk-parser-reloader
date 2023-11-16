module Yggdrasil.Config.Data where
import Prelude hiding (readFile)
import Data.ByteString

getHeader :: IO ByteString
getHeader = readFile "./config/header.conf"
