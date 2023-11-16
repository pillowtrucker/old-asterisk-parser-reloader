{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Yggdrasil.Database (getPrefixesWithBLF,liftExtractMysql,getAllContexts,justByteStringResult,getSIPPeersForPrefix,findServerForFlatContext) where
import Yggdrasil.Util(cxNameToPrefix,b2T,t2B)
import qualified System.IO.Streams as S
import "mysql-haskell" Database.MySQL.Base
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Monad
import Yggdrasil.Types
import qualified Control.Monad.Parallel as Par
mysqlText2MaybeText :: MySQLValue -> Maybe T.Text
mysqlText2MaybeText x = case x of
  MySQLText y -> Just y
  _ -> Nothing

mysqlInt2MaybeInt :: MySQLValue -> Maybe Integer
mysqlInt2MaybeInt x = case x of
  MySQLInt32 y -> Just $ toInteger y
  _ -> Nothing


findServerForFlatContext
  :: MyConfig B.ByteString -> B.ByteString -> IO (Maybe Integer)
findServerForFlatContext c cn = (getServerFromPrefix c . cxNameToPrefix) cn >>= return . justIntResult . head . head



execBasicQueryOnDB
  :: B.ByteString
     -> B.ByteString
     -> MyConfig B.ByteString
     -> Query
     -> IO [[MySQLValue]]
execBasicQueryOnDB theDb d c q = do
  conn <- connect
            defaultConnectInfo {ciHost = B.unpack $ d, ciUser = dbUser c, ciPassword = dbPass c, ciDatabase = theDb}
  (_, rs) <- query_ conn q
  S.toList rs


execQueryOnDB
  :: QueryParam p =>
     B.ByteString
     -> B.ByteString
     -> MyConfig B.ByteString
     -> Query
     -> [p]
     -> IO [[MySQLValue]]
execQueryOnDB theDb d c q p = do
  conn <- connect
            defaultConnectInfo {ciHost = B.unpack $ d, ciUser = dbUser c, ciPassword = dbPass c, ciDatabase = theDb}
  (_, rs) <- query conn q p
  S.toList rs



execBasicQueryOnVoipDB
  :: MyConfig B.ByteString -> Query -> IO [[MySQLValue]]
execBasicQueryOnVoipDB c = execBasicQueryOnDB (voipDb c) (voipDbHost c) c


execBasicQueryOnAstDB
  :: MyConfig B.ByteString -> Query -> IO [[MySQLValue]]
execBasicQueryOnAstDB c = execBasicQueryOnDB (astDb c) (astDbHost c) c



execQueryOnVoipDB
  :: QueryParam p =>
     MyConfig B.ByteString -> Query -> [p] -> IO [[MySQLValue]]
execQueryOnVoipDB c = execQueryOnDB (voipDb c) (voipDbHost c) c



execQueryOnAstDB
  :: QueryParam p =>
     MyConfig B.ByteString -> Query -> [p] -> IO [[MySQLValue]]
execQueryOnAstDB c = execQueryOnDB (astDb c) (astDbHost c) c


getAllContexts :: MyConfig B.ByteString -> IO [[MySQLValue]]
getAllContexts c = execBasicQueryOnAstDB c "select distinct(context) from extensions"



getSIPPeersForPrefix
  :: MyConfig B.ByteString -> T.Text -> IO [[MySQLValue]]
getSIPPeersForPrefix c p = execQueryOnAstDB c "select name from sip where name like concat(?,'%')" [MySQLText p]


getPrefixesWithBLF :: MyConfig B.ByteString -> IO [[MySQLValue]]
getPrefixesWithBLF c = execBasicQueryOnVoipDB c "select c.prefix from customers c inner join customer_config cc on c.custref=cc.custref where cc.BLF='yes'"


firstText :: [MySQLValue] -> Maybe T.Text
firstText = mysqlText2MaybeText . head

justByteStringResult :: [MySQLValue] -> Maybe B.ByteString
justByteStringResult = liftM t2B . firstText

justIntResult :: MySQLValue -> Maybe Integer
justIntResult = mysqlInt2MaybeInt


liftExtractMysql :: IO [[MySQLValue]] -> IO [Maybe B.ByteString]
liftExtractMysql a = a >>= Par.mapM (return . justByteStringResult)


getServerFromPrefix
  :: MyConfig B.ByteString -> Prefix -> IO [[MySQLValue]]
getServerFromPrefix c p = (return $ MySQLText $ b2T $ getPrefixName p) >>= \p' -> execQueryOnVoipDB c "select cc.server from customer_config cc inner join customers c on cc.custref=c.custref where c.prefix = lower(?) limit 1" (p':[])
