{-# LANGUAGE OverloadedStrings #-}
module Yggdrasil.Config where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Ini.Config
import System.Directory (getCurrentDirectory)
import Yggdrasil.Types
import Prelude hiding (readFile)


defaultConfigPath :: IO [Char]
defaultConfigPath = getCurrentDirectory >>= return . (++ "/config/reloader.conf")
voipDbHost' :: SectionParser T.Text
voipDbHost' = field "voipdbhost"
astDbHost' :: SectionParser T.Text
astDbHost' = field "astdbhost"
dbUser' :: SectionParser T.Text
dbUser' = field "dbuser"
dbPass' :: SectionParser T.Text
dbPass' = field "dbpass"
astDb' :: SectionParser T.Text
astDb' = field "astdb"
voipDb' :: SectionParser T.Text
voipDb' = field "voipdb"
mqHost' :: SectionParser T.Text
mqHost' = field "mqhost"
mqVhost' :: SectionParser T.Text
mqVhost' = field "mqvhost"
mqUser' :: SectionParser T.Text
mqUser' = field "mquser"
mqPass' :: SectionParser T.Text
mqPass' = field "mqpass"
extConfPath' :: SectionParser T.Text
extConfPath' = field "extconfpath"
extNewConfPath' :: SectionParser T.Text
extNewConfPath' = extConfPath'
parseMyConfig :: IniParser (MyConfig T.Text)
parseMyConfig = do
  section "reloader" $ do
    vdbh <- voipDbHost'
    adbh <- astDbHost'
    dbu <- dbUser'
    dbp <- dbPass'
    adb <- astDb'
    vdb <- voipDb'
    mqh <- mqHost'
    mqvh <- mqVhost'
    mqu <- mqUser'
    mqp <- mqPass'
    ecp <- extConfPath'
    return $ MyConfig vdbh adbh dbu dbp adb vdb mqh mqvh mqu mqp ecp
    
readMyConfig :: IO (Either String (MyConfig T.Text))
readMyConfig = defaultConfigPath >>= TIO.readFile >>= \fc -> return $ parseIniFile fc parseMyConfig
