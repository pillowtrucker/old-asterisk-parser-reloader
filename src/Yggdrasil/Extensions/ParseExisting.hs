module Yggdrasil.Extensions.ParseExisting (testParsing) where
import Yggdrasil.Util((++),bFilterOutgoing,bNotExternal,appendDynamic,appendSupport,appendRec,appendHints,appendExt)
import Yggdrasil.Types (BareContext (..),HintEntry (..),MyConfig (..),AstContext (..), ConfigFile (..),ConfigPreamble (..), BareConfig (..), ContextEntry (..), ContextInclude (..),flatCompare,flatCompareBackwards,flatCompareBLF, RedirectEntry (..))
import Yggdrasil.Config.Data
import Yggdrasil.Database (findServerForFlatContext)
import qualified Data.ByteString.Char8 as B
import Text.Megaparsec
import Text.Megaparsec.Char
import Prelude hiding ((++),readFile)
import Yggdrasil.Extensions (testAllContexts, mc2Other, t2BS)
import Control.Monad
import Data.List ((\\),sortBy,elemIndex)
import qualified Control.Monad.Parallel as Par
import Yggdrasil.Extensions.MessageQueue hiding ((++))
import qualified Network.AMQP as Rabbit
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM
import Data.Maybe (fromMaybe)
import Data.Void
import System.IO.Strict (readFile)
import qualified Data.Text as T

type Parser = Parsec Void String


expectedPreamble :: IO [Char]
expectedPreamble = getHeader >>= return . B.unpack

leftSquareBrace :: Parser Char
leftSquareBrace = char '['


rightSquareBrace :: Parser Char
rightSquareBrace = char ']'

contextChar :: Parser Char
contextChar = alphaNumChar <|> char '-'

contextHeader :: Parser BareContext
contextHeader = do
  theContext <- bracedVal $ many contextChar
  _ <- eol
  return $ BareContext $ B.pack theContext


bracedVal :: Parser a -> Parser a
bracedVal = between leftSquareBrace rightSquareBrace

dynamicRedirect :: Parser ContextEntry
dynamicRedirect = do
  me <- string "switch => Realtime/@\n"
  return $ ContextRedirectEntry $ RedirectEntry $ B.pack me

anInclude :: Parser ContextEntry
anInclude = do
  _ <- string "include => "
  whatInclude <- someTill contextChar eol
  let whatIncludeB = B.pack whatInclude
  return $ ContextIncludeEntry $
    if (B.pack "-ext" `B.isSuffixOf` whatIncludeB) then
      ExtInclude whatIncludeB
    else if (whatIncludeB == B.pack "support") then
      SupportInclude whatIncludeB
    else if (B.pack "-recordings" `B.isSuffixOf` whatIncludeB) then
      RecInclude whatIncludeB
    else
      OtherInclude whatIncludeB

comma :: Parser Char
comma = char ','

aHintLine :: Parser ContextEntry
aHintLine = do
  _ <- string "exten => "
  theNumber <- liftM B.pack $ someTill digitChar comma
  string "hint" >> comma >> string "SIP/" >> return ()
  thePeer <- liftM B.pack $ someTill alphaNumChar eol
  return $ ContextHintEntry $ HintEntry $ theNumber ++ B.pack ",hint,SIP/" ++ thePeer

theEnd :: Parser ()
theEnd = (try eol >> return ()) <|> try eof

theEntries :: Parser [ContextEntry]
theEntries = someTill (try anInclude <|> try aHintLine <|> dynamicRedirect) $ theEnd

filterHints :: ContextEntry -> Bool
filterHints e = case e of
  ContextHintEntry _ -> True
  _ -> False

filterDynamics :: ContextEntry -> Bool
filterDynamics e = case e of
  ContextRedirectEntry _ -> True
  _ -> False

isExt :: ContextInclude -> Bool
isExt inc = case inc of
  ExtInclude _ -> True
  _ -> False

isRec :: ContextInclude -> Bool
isRec inc = case inc of
  RecInclude _ -> True
  _ -> False


pBuildOutgoingSingle
  :: BareContext -> Bool -> Bool -> Maybe [HintEntry] -> AstContext
pBuildOutgoingSingle bare hasExt hasRec hints =
  let almostThere = OutgoingContext bare hasExt hasRec hints $ getBareContextName bare
  in 
    appendHints . (if hasRec then appendRec else id) . (if hasExt then appendExt else id) . appendSupport . appendDynamic $ almostThere

pBuildExtSingle :: BareContext -> AstContext
pBuildExtSingle bare =
  let almostThere = ExtContext bare $ getBareContextName bare
  in
    appendDynamic $ almostThere

pBuildIncomingSingle :: BareContext -> AstContext
pBuildIncomingSingle bare =
  let almostThere = IncomingContext bare $ getBareContextName bare
  in
    appendDynamic $ almostThere

fullContext :: Parser AstContext
fullContext = do
  theBare <- contextHeader
  entries <- theEntries
  let myName = getBareContextName theBare
      myHintEntries = filter filterHints entries
      myHints = map getHint $ myHintEntries
      myRedirects = filter filterDynamics entries
      myIncludes = (entries \\ myHintEntries) \\ myRedirects
      myIncludeEntries = map getInclude $ myIncludes
      pCHasExt = (length $ filter (isExt) myIncludeEntries) > 0
      pCHasRec = (length $ filter (isRec) myIncludeEntries) > 0
      pCHasHints = length myHints > 0
  if bFilterOutgoing myName && bNotExternal myName then
      return $ pBuildOutgoingSingle theBare pCHasExt pCHasRec (if pCHasHints then Just myHints else Nothing)
  else if bFilterOutgoing myName then do
      return $ pBuildExtSingle theBare
  else do
      return $ pBuildIncomingSingle theBare


thePreamble :: String -> Parser ConfigPreamble
thePreamble ep = do
  p1 <- string ep
  return $ ConfigPreamble $ B.pack p1

pFullConfig :: String -> Parser ConfigFile
pFullConfig ep = do
  myPreamble <- thePreamble ep
  _ <- eol
  myContexts <- some fullContext
  _ <- eof
  return $ ConfigFile myPreamble myContexts


doTheParse :: MyConfig T.Text -> IO (Either (ParseError Char Void) ConfigFile)
doTheParse c = do
  myFile <- readFile (T.unpack $ extConfPath c)
  ep <- expectedPreamble
  return $ parse (pFullConfig ep) (T.unpack $ extConfPath c) myFile


testParsing :: MyConfig T.Text -> IO ()
testParsing mc = do
  eParsed <- doTheParse mc
  eBuilt <- testAllContexts
  case eParsed of
    Left err -> putStr $ show err
    Right c -> do
      let forwardChanges = flatCompare c eBuilt
          backwardChanges = flatCompareBackwards c eBuilt
          forwardBLFChanges = flatCompareBLF c eBuilt
      B.putStrLn $ B.pack "BLF Changes: " ++ (B.pack $ show forwardBLFChanges)
      B.putStrLn $ B.pack "Added: " ++ (B.pack $ show forwardChanges)
      B.putStrLn $ B.pack "Removed: " ++ (B.pack $ show backwardChanges)
      
      (conn,chan) <- initMq mc
      let sendMsg' = sendMsg chan
      _ <- sendMsg' $ BL.pack "Parser initialised"
      buildThenPublishChanges (mc2Other mc t2BS) chan (forwardChanges,backwardChanges)
      buildThenPublishChanges (mc2Other mc t2BS) chan (BareConfig (map (\(contxt,_) -> contxt) forwardBLFChanges),BareEmpty)
      Rabbit.closeConnection conn
      return ()


sendMsg :: Rabbit.Channel -> BL.ByteString -> IO (Maybe Int)
sendMsg chan m = Rabbit.publishMsg chan theExchangeName theChanKey (Rabbit.newMsg {Rabbit.msgBody = m, Rabbit.msgDeliveryMode = Just Rabbit.NonPersistent})



translateOccurencesToDelay
  :: (Eq t1, Ord t) => M.Map t1 t -> [(t1, Integer)]
translateOccurencesToDelay cm =
  let sorted = sortBy (\(s,o) (s1,o1) -> compare o o1) $! M.assocs cm
      delay :: Int
      delay = 15
  in
    map (\p@(s,o) -> (s, (toInteger $ (delay * (fromMaybe 0 $ elemIndex p sorted))))) sorted

filterOutMismatch :: Maybe t -> Bool
filterOutMismatch m = case m of
                        Just _ -> True
                        Nothing -> False

justSNameToTuple :: (Num t, Num t1) => Maybe t1 -> (t1, t)
justSNameToTuple m = case m of
  Just x -> (x,1)
  Nothing -> (0,0)


buildChangesCountMap
  :: Num a =>
     MyConfig B.ByteString -> [B.ByteString] -> IO (M.Map Integer a)
buildChangesCountMap mc cl = do
  servers <- Par.mapM (findServerForFlatContext mc) cl
  let fservers = filter filterOutMismatch servers
  tuples <- Par.mapM (return . justSNameToTuple) fservers
  return $! M.fromList tuples


mergeBackAndForwardChangesMap
  :: M.Map Integer Integer
     -> M.Map Integer Integer -> M.Map Integer Integer
mergeBackAndForwardChangesMap = MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched $ \_ a b -> a + b)



buildThenPublishChanges
  :: MyConfig B.ByteString
     -> Rabbit.Channel
     -> (BareConfig B.ByteString, BareConfig B.ByteString)
     -> IO ()
buildThenPublishChanges mc chan changes = do
  case changes of
    (BareConfig cs1, BareConfig cs2) -> do
      cm1 <- buildChangesCountMap mc cs1
      cm2 <- buildChangesCountMap mc cs2
      let cmm = M.fromList . translateOccurencesToDelay $! mergeBackAndForwardChangesMap cm1 cm2
      publishChanges chan cmm
      return ()
    (BareConfig cs1, BareEmpty) -> do
      cm1 <- buildChangesCountMap mc cs1
      publishChanges chan $! (M.fromList $! translateOccurencesToDelay cm1)
      return ()
    (BareEmpty,BareConfig cs2) -> do
      cm2 <- buildChangesCountMap mc cs2
      publishChanges chan $! (M.fromList $! translateOccurencesToDelay cm2)
      return ()
    (BareEmpty,BareEmpty) -> return ()
  Rabbit.waitForConfirms chan
  
  return ()
publishChanges
  :: (Show a1, Show a) =>
     Rabbit.Channel -> M.Map a1 a -> IO (M.Map a1 (Maybe Int))
publishChanges chan cmap =
  let sendMsg' = sendMsg chan
  in
  M.traverseWithKey (\k v -> sendMsg' ((BL.pack "RELOAD ") ++++ (BL.pack $ show k) ++++ (BL.pack " ") ++++ (BL.pack $ show v))) cmap

(++++) :: BL.ByteString -> BL.ByteString -> BL.ByteString
a ++++ b = BL.append a b
