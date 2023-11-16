{-# LANGUAGE OverloadedStrings #-}
module Yggdrasil.Util (t2B,(++),m2BF,append',(+++),liftUnMaybe,lOfM2L,b2T,bFilterOutgoing,bNotExternal,contextToPrefix,appendDynamic,appendExt,appendHints,appendRec,appendSupport,cxNameToPrefix) where
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Yggdrasil.Types
import qualified Data.List as L
import Data.List (foldl')
import Prelude hiding ((++))
import qualified Control.Monad.Parallel as Par

getNameFromAstContext :: AstContext -> B.ByteString
getNameFromAstContext = getBareContextName . getContextBare


contextNameToConfigHeader :: B.ByteString -> B.ByteString
contextNameToConfigHeader = newLine' . newLine . rightSquareBracket . leftSquareBracket

leftSquareBracket :: B.ByteString -> B.ByteString
leftSquareBracket = B.append "["

rightSquareBracket :: B.ByteString -> B.ByteString
rightSquareBracket = append' "]"

newLine' :: B.ByteString -> B.ByteString
newLine' = B.cons '\n'

newLine :: B.ByteString -> B.ByteString
newLine = append' "\n"


appendSupport :: AstContext -> AstContext
appendSupport c =
  let theNewBody = append' "include => support\n" . getContextBody $ c
  in
    case c of
      OutgoingContext b e r h _ -> OutgoingContext b e r h theNewBody
      _ -> c

appendExt :: AstContext -> AstContext
appendExt c =
  let theBody = getContextBody c
      myName = getNameFromAstContext c
      hasExt = outContextHasExt c
      theNewBody = append' ("include => " ++ myName ++ "-ext\n") $ theBody
  in
    case c of
      OutgoingContext b e r h _ -> if (hasExt) then OutgoingContext b e r h theNewBody else c
      _ -> c

appendRec :: AstContext -> AstContext
appendRec c =
  let theBody = getContextBody c
      myPrefix = getPrefixName $ contextToPrefix $ getContextBare c
      hasRec = outContextHasRecordings c
      theNewBody = append' ("include => " ++ myPrefix ++ "-recordings\n") $ theBody
  in
    case c of
      OutgoingContext b e r h _ -> if (hasRec) then OutgoingContext b e r h theNewBody else c
      _ -> c


appendHints :: AstContext -> AstContext
appendHints c =
  let theBody = getContextBody c
      hasHints = m2BF (\_ -> True) myHints
      myHints = getContextHints c
      theNewBody = case myHints of
        Just mh -> foldl' (\a b -> a ++ (getHintBody b) ++ "\n") theBody mh
        Nothing -> theBody
  in
    case c of
      OutgoingContext b e r h _ -> if (hasHints) then OutgoingContext b e r h theNewBody else c
      _ -> c
    


appendDynamic :: AstContext -> AstContext
appendDynamic c =
  let theNewBody = append' "switch => Realtime/@\n" . contextNameToConfigHeader . getContextBody $ c
  in case c of
    IncomingContext b _ -> IncomingContext b theNewBody
    OutgoingContext b e r h _ -> OutgoingContext b e r h theNewBody
    ExtContext b _ -> ExtContext b theNewBody

(++) :: B.ByteString -> B.ByteString -> B.ByteString
a ++ b = a `B.append` b

m2BF :: (t -> Bool) -> Maybe t -> Bool
m2BF f m = case m of
  Just m' -> f m'
  Nothing -> False

t2B :: T.Text -> B.ByteString
t2B = B.pack . T.unpack


b2T :: B.ByteString -> T.Text
b2T = T.pack . B.unpack

lOfM2L' :: [Maybe B.ByteString] -> IO [B.ByteString]
lOfM2L' b = Par.mapM (\a -> return $ case a of
             Just a' -> a'
             Nothing -> "") $ filter (\a -> case a of
                                        Just _ -> True
                                        Nothing -> False) b
           
lOfM2L :: [Maybe B.ByteString] -> [B.ByteString]
lOfM2L b = map (\a -> case a of
             Just a' -> a'
             Nothing -> "") $ filter (\a -> case a of
                                        Just _ -> True
                                        Nothing -> False) b

liftUnMaybe :: IO [Maybe B.ByteString] -> IO [B.ByteString]
liftUnMaybe a = a >>= lOfM2L' >>= return


(+++) :: [a] -> [a] -> [a]
a +++ b = (L.++) a b


append' :: B.ByteString -> B.ByteString -> B.ByteString
append' = flip B.append


bFilterOutgoing :: B.ByteString -> Bool
bFilterOutgoing = B.isInfixOf "-local"

bNotExternal :: B.ByteString -> Bool
bNotExternal = not . B.isSuffixOf "-ext"

contextToPrefix :: BareContext -> Prefix
contextToPrefix bc = let test = getBareContextName bc in
  if "sus-" `B.isPrefixOf` test then
  Prefix $ B.take 4 $ B.drop 4 test
  else Prefix $ B.take 4 test

cxNameToPrefix :: B.ByteString -> Prefix
cxNameToPrefix cn = if "sus-" `B.isPrefixOf` cn then
                      Prefix $ B.take 4 $ B.drop 4 cn
                    else Prefix $ B.take 4 cn
