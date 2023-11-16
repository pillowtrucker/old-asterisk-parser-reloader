{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yggdrasil.Types where
import qualified Data.ByteString.Char8 as B
import qualified Data.ListLike as LL
import qualified Data.List as L

import Data.String (IsString)
import Data.Maybe (isNothing)
data Prefix = Prefix {getPrefixName :: B.ByteString}
data ConfigPreamble = ConfigPreamble {getPreambleContent :: B.ByteString}
  deriving (Eq, Show)
data HintEntry = HintEntry {getHintBody :: B.ByteString}
  deriving (Eq, Show)
data BareContext = BareContext {getBareContextName :: B.ByteString}
  deriving (Eq, Show)
data (IsString a) => MyConfig a = MyConfig {voipDbHost :: a,
                                            astDbHost :: a,
                                            dbUser :: a,
                                            dbPass :: a,
                                            astDb :: a,
                                            voipDb :: a,
                                            mqHost :: a,
                                            mqVhost :: a,
                                            mqUser :: a,
                                            mqPass :: a,
                                            extConfPath :: a
                                           }
data AstContext =
  OutgoingContext {getContextBare :: BareContext,
                    outContextHasExt :: Bool,
                    outContextHasRecordings :: Bool,
                    getContextHints :: Maybe [HintEntry],
                    getContextBody :: B.ByteString} |
  IncomingContext {getContextBare:: BareContext, getContextBody :: B.ByteString } |
  ExtContext {getContextBare :: BareContext, getContextBody :: B.ByteString }
  deriving (Eq,Show)
data ConfigFile = ConfigFile {getPreamble :: ConfigPreamble,
                              getContexts :: [AstContext] }
  deriving (Eq, Show)


data ContextInclude = SupportInclude B.ByteString | ExtInclude B.ByteString | RecInclude B.ByteString | OtherInclude B.ByteString
  deriving Eq
data RedirectEntry = RedirectEntry B.ByteString
  deriving Eq
data ContextEntry = ContextHintEntry {getHint :: HintEntry} | ContextIncludeEntry {getInclude :: ContextInclude } | ContextRedirectEntry {getRedirect :: RedirectEntry}
  deriving Eq


data BareConfig a = BareConfig [a] | BareEmpty
  deriving Show


instance LL.FoldableLL (BareConfig a) a where
  foldl f b (BareConfig a) = L.foldl f b a
  foldl _ b BareEmpty = b
  foldr f b (BareConfig a) = L.foldr f b a
  foldr _ b BareEmpty = b
  
instance Monoid (BareConfig a) where
  mempty = BareEmpty
  mappend (BareEmpty) (BareConfig a) = BareConfig a
  mappend (BareConfig a) (BareConfig b) = BareConfig (a ++ b)
  mappend (BareConfig a) BareEmpty = BareConfig a
  mappend BareEmpty BareEmpty = BareEmpty
  
instance (Monoid a) => LL.ListLike (BareConfig a) a where
  singleton a = BareConfig [a]
  head (BareConfig a) = L.head a
  head BareEmpty = mempty
  tail (BareConfig a) = BareConfig (L.tail a)
  tail BareEmpty = mempty
  null (BareConfig a) = L.null a
  null BareEmpty = True
  
buildFlat :: ConfigFile -> BareConfig B.ByteString
buildFlat (ConfigFile b c) = BareConfig ((getPreambleContent b):(map (getBareContextName . getContextBare) c))


buildFlatBLFList :: ConfigFile -> [(B.ByteString, Bool)]
buildFlatBLFList (ConfigFile _ c) = map (\theContext -> (getBareContextName $ getContextBare theContext, isNothing $ getContextHints theContext) ) (filter filterToOnlyOutgoing c)

filterToOnlyOutgoing :: AstContext -> Bool
filterToOnlyOutgoing c = case c of
                           OutgoingContext _ _ _ _ _ -> True
                           _ -> False
(\\) :: Eq a => [a] -> [a] -> [a]
a \\ b = (L.\\) a b
infixr 9 \\

flatCompareBLF :: ConfigFile -> ConfigFile -> [(B.ByteString,Bool)]
flatCompareBLF a b = (buildFlatBLFList b) \\ (buildFlatBLFList a)

flatCompareBLFBackwards
  :: ConfigFile -> ConfigFile -> [(B.ByteString, Bool)]
flatCompareBLFBackwards = flip flatCompareBLF

flatCompare
  :: ConfigFile -> ConfigFile -> BareConfig B.ByteString
flatCompare a b =
  let theDiff = (buildFlat b) `LL.deleteFirsts` (buildFlat a)
  in
    if LL.null theDiff then BareEmpty else theDiff

flatCompareBackwards
  :: ConfigFile -> ConfigFile -> BareConfig B.ByteString
flatCompareBackwards = flip flatCompare
