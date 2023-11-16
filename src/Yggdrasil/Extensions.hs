{-# LANGUAGE OverloadedStrings #-}
module Yggdrasil.Extensions (testAllContexts,t2BS,mc2Other) where
import Yggdrasil.Config
import Yggdrasil.Config.Data
import qualified Data.ByteString.Char8 as B
import Prelude hiding ((++),putStrLn)
import System.IO(openFile, IOMode (WriteMode),hClose)
import Control.Monad
import Data.List ((\\))
import Yggdrasil.Util(liftUnMaybe,contextToPrefix,b2T,(++),bFilterOutgoing,bNotExternal,appendDynamic,appendExt,appendHints,appendRec,appendSupport,(+++))
import Yggdrasil.Database
import Yggdrasil.Types (BareContext (..),HintEntry (..),MyConfig (..),AstContext (..), Prefix (..),ConfigFile (..),ConfigPreamble (..))
import qualified Control.Monad.Parallel as Par
import qualified Data.Text as T
import Data.String (IsString)
prefixToDefaultContext :: B.ByteString -> B.ByteString
prefixToDefaultContext p = p ++ "-local"

--exten => 105,hint,SIP/ABBY105
peerToBLFHint :: B.ByteString -> B.ByteString
peerToBLFHint p = let num = B.drop 4 p in "exten => " ++ num ++ ",hint,SIP/" ++ p

extExistsForContext
  :: Foldable t => t B.ByteString -> BareContext -> Bool
extExistsForContext lc c = (getBareContextName c ++ "-ext") `elem` lc

recordingsExistForContext
  :: Foldable t => t B.ByteString -> BareContext -> Bool
recordingsExistForContext lc c = ((getPrefixName $ contextToPrefix c) ++ "-recordings") `elem` lc


prefixToHints
  :: MyConfig B.ByteString -> Prefix -> IO (Maybe [HintEntry])
prefixToHints c p = do
   peers <- liftUnMaybe . liftExtractMysql $ getSIPPeersForPrefix c (b2T $ getPrefixName p)
   return $ if ((length peers) > 0) then
     Just $ map (HintEntry . peerToBLFHint) peers
     else Nothing


buildOutgoingSingle
  :: (Foldable t, Foldable t1, Foldable t2) =>
     MyConfig B.ByteString
     -> t2 BareContext
     -> t1 BareContext
     -> t BareContext
     -> BareContext
     -> IO AstContext
buildOutgoingSingle c withext withblf withrecordings outcontext =
  let hasExt = outcontext `elem` withext
      hasBlf = outcontext `elem` withblf
      hasRecordings = outcontext `elem` withrecordings
  in do
    myHints <- if (hasBlf) then prefixToHints c $ contextToPrefix outcontext else return Nothing
    return $ OutgoingContext outcontext hasExt hasRecordings myHints (getBareContextName outcontext)

buildIncomingSingle :: BareContext -> AstContext
buildIncomingSingle bc = IncomingContext bc $ getBareContextName bc

buildExternalSingle :: BareContext -> AstContext
buildExternalSingle bc = ExtContext bc $ getBareContextName bc



buildContexts :: MyConfig B.ByteString -> IO ConfigFile
buildContexts c = do
  allCont <- liftUnMaybe . liftExtractMysql $ getAllContexts c
  preamble <- liftM ConfigPreamble getHeader
  blfPrefixes <- liftUnMaybe . liftExtractMysql $ getPrefixesWithBLF c
  let outOnly = map BareContext $ filter (\c -> bFilterOutgoing c && bNotExternal c) allCont
      extOnly = map BareContext $ filter (\c -> bFilterOutgoing c && (not $ bNotExternal c)) allCont
      contextsWithExt = filter (extExistsForContext allCont) outOnly
  contextsWithBLF <- Par.mapM (return . BareContext . prefixToDefaultContext) blfPrefixes
  let contextsWithRec = filter (recordingsExistForContext allCont) outOnly
  builtOutgoing <- Par.mapM (buildOutgoingSingle c contextsWithExt contextsWithBLF contextsWithRec) outOnly
  allCont' <- Par.mapM (return . BareContext ) allCont
  builtIncoming <- Par.mapM (return . buildIncomingSingle) $ ((allCont' \\ outOnly) \\ extOnly)
  builtExternal <- Par.mapM (return . buildExternalSingle) $ extOnly
  return $ ConfigFile preamble (builtOutgoing +++ builtIncoming +++ builtExternal)



appendAllDynamic :: ConfigFile -> IO ConfigFile
appendAllDynamic = applyAppendAll appendDynamic


appendAllSupport :: ConfigFile -> IO ConfigFile
appendAllSupport = applyAppendAll appendSupport


appendAllExt :: ConfigFile -> IO ConfigFile
appendAllExt = applyAppendAll appendExt


appendAllHints :: ConfigFile -> IO ConfigFile
appendAllHints = applyAppendAll appendHints


appendAllRecordings :: ConfigFile -> IO ConfigFile
appendAllRecordings = applyAppendAll appendRec


applyAppendAll
  :: Par.MonadParallel m =>
     (AstContext -> AstContext) -> ConfigFile -> m ConfigFile
applyAppendAll f cf = do
  newContexts <- Par.mapM (return . f) (getContexts cf)
  let thePreamble = getPreamble cf
  return $ ConfigFile thePreamble newContexts
                         

buildFinalConfig :: MyConfig B.ByteString -> IO ConfigFile
buildFinalConfig c = do
  initial <- buildContexts c
  withDynamic <- appendAllDynamic initial
  withSupport <- appendAllSupport withDynamic
  withExt <- appendAllExt withSupport
  withRec <- appendAllRecordings withExt
  withHints <- appendAllHints withRec
  return withHints


writeTheConfig :: MyConfig FilePath -> ConfigFile -> IO ()
writeTheConfig mc fc = do
  let preamble = getPreambleContent $ getPreamble fc
      contexts = map (getContextBody) $ getContexts fc
  B.appendFile (extConfPath mc) preamble
  mapM_ (B.appendFile (extConfPath mc)) contexts

t2S :: T.Text -> String
t2S = T.unpack
t2BS :: T.Text -> B.ByteString
t2BS = B.pack . T.unpack

mc2Other
  :: (IsString t, IsString a) =>
     MyConfig t -> (t -> a) -> MyConfig a
mc2Other (MyConfig a b c d e f g h i j k) f' = MyConfig (f' a) (f' b) (f' c) (f' d) (f' e) (f' f) (f' g) (f' h) (f' i) (f' j) (f' k)


testAllContexts :: IO ConfigFile
testAllContexts = do
  mc <- readMyConfig
  case mc of
    Right c -> do
      let mcs = mc2Other c t2S
      finalConfig <- buildFinalConfig (mc2Other c t2BS)
      nukeConfig mcs >> writeTheConfig mcs finalConfig
      return finalConfig
    Left e -> fail e


nukeConfig :: MyConfig FilePath -> IO ()
nukeConfig c = openFile (extConfPath c) WriteMode >>= hClose >> return ()
