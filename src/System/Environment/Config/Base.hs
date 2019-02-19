{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config.Base where

import Control.Monad.State (StateT(..))
import Data.Char (toLower, isSpace)
import Data.Aeson (Value(..), FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Control.Monad.Reader
import Control.Monad.Catch
import Xeno.Types (XenoException)
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Xeno.DOM as X
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M

-- TODO: generic implementation
class FromConfig a where
    fromConfig :: ConfigNode -> Reader Options (A.Result a)
    default fromConfig :: FromJSON a => ConfigNode -> Reader Options (A.Result a)
    fromConfig = return . A.fromJSON . A.toJSON

type EnvSource o a = ReaderT o IO a
type EnvReader o s a = ReaderT o (StateT s IO) a

data Options = Options {
        envNameKey :: String
    }

data ConfigNode = ConfigNode Value (M.Map String ConfigNode)
    deriving (Show, Eq)

instance Semigroup ConfigNode where
    (<>) (ConfigNode v2 p2) (ConfigNode v1 p1) = ConfigNode (mergeVal v2 v1) $ H.unionWith (<>) p2 p1

instance Monoid ConfigNode where
    mempty = ConfigNode Null H.empty
    mappend = (<>)

instance ToJSON ConfigNode where
    toJSON (ConfigNode v m) 
            | H.null m = v
            | otherwise = Object $ H.fromList $ H.foldrWithKey toPair mempty m
        where toPair k c acc = (T.pack k, A.toJSON c) : acc

newtype JsonSourceException = AesonError String 
    deriving (Show)

instance Exception JsonSourceException where
    displayException (AesonError e) = e

newtype YamlSourceException = YamlError YL.ParseException
    deriving (Show)

instance Exception YamlSourceException where
    displayException e = case e of
        YamlError e' -> displayException e'

newtype XmlSourceException = XenoError XenoException
    deriving (Show)

instance Exception XmlSourceException where
    displayException (XenoError e) = displayException e

newtype IniSourceException = IniError String
    deriving (Show)

instance Exception IniSourceException where
    displayException (IniError e) = e

data ConfigGetException = 
      KeyNotFoundError String
    | ParseValueError String 
    deriving (Show)

instance Exception ConfigGetException where
    displayException e = case e of
        KeyNotFoundError k -> "key " ++ k ++ " not found in configuration"
        ParseValueError s -> s

-- normalizeKey :: String -> String
-- normalizeKey ('_':'_':cs) = '.' : normalizeKey (dropWhile (== '_') cs)
-- normalizeKey (c:cs) = toLower c : normalizeKey cs
-- normalizeKey [] = []

-- filterEnv :: [String] -> [(String, String)] -> [(String, String)]
-- filterEnv prefixes envVars = map envPair $ filter keyMatchesPrefix envVars
--     where
--         keyMatchesPrefix (k, _) = any ((`isPrefixOf` lcase k) . tailIfTilde . lcase) prefixes
--         tailIfTilde ('~':cs) = cs
--         tailIfTilde cs = cs
--         envPair (k, v) = let
--                 isLongestMatched lk ('~':p) Nothing  | lcase p `isPrefixOf` lk = Just p
--                                                      | otherwise = Nothing
--                 isLongestMatched lk ('~':p) (Just l) | lcase p `isPrefixOf` lk && length p > length l = Just p
--                                                      | otherwise = Just l
--                 isLongestMatched _ _ l = l
--                 stripLongestPrefix lk = fromMaybe lk $
--                     foldr (isLongestMatched lk) Nothing prefixes >>= \p ->
--                         stripPrefix (lcase p) lk
--             in (normalizeKey $ stripLongestPrefix $ lcase k, v)

-- argToPair :: String -> (String, String)
-- argToPair ('-':'-':arg) = argToPair arg
-- argToPair arg = let (k, v) = splitAtEl '=' arg 
--                 in (normalizeKey k, v)

-- mapArgs :: [String] -> [(String, String)]
-- mapArgs (a1:a2:args') | a1 == "--" = mapArgs (a2:args')
--                       | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args'
--                       | otherwise = argToPair a1 : mapArgs (a2:args')
--     where wantsArg ('-':'-':a) = '=' `notElem` a
--           wantsArg _ = False
--           isArg ('-':'-':_) = False 
--           isArg a = '=' `notElem` a
-- mapArgs (arg:args') = argToPair arg : mapArgs args'
-- mapArgs [] = []

runEnvReader :: MonadIO m => Options -> ConfigNode -> EnvReader Options ConfigNode a -> m ConfigNode
runEnvReader e s = liftIO . (`execStateT` s) . (`runReaderT` e)

valueToConfig :: Value -> Reader Options ConfigNode
valueToConfig = undefined
    
    -- valueToConfig'
    -- where
    --     valueToConfig' (Object o) = H.foldrWithKey setProp mempty o
    --         where setProp k v = setC (T.unpack k) (valueToConfig' v)
    --     valueToConfig' v = ConfigNode v H.empty

pairsToConfig :: [(String, String)] -> Reader Options ConfigNode
pairsToConfig = undefined
    
    -- foldr setPair mempty
    -- where setPair (path, v) = swap (mergeVal $ tryDecodeS v) path

filterPairsToConfig :: [String] -> [(String, String)] -> Reader Options ConfigNode
filterPairsToConfig = undefined

iniToConfig :: I.Ini -> Reader Options ConfigNode
iniToConfig ini = undefined

    --     let globals = setPairs mempty $ I.iniGlobals ini
    --     in H.foldrWithKey setSection globals $ I.iniSections ini
    -- where 
    --     setPairs = foldr setPair
    --     setSection section pairs c = setPairs (swap id (T.unpack section) c) pairs
    --     setPair (path, v) = swap (mergeVal $ tryDecodeT v) (T.unpack path)

xmlToConfig :: X.Node -> Reader Options ConfigNode
xmlToConfig node = do
    config <- nodeConfig node
    return $ setC (B.unpack $ X.name node) config mempty

xmlContentToConfig :: X.Content -> Reader Options ConfigNode
xmlContentToConfig (X.Element node) = xmlToConfig node
xmlContentToConfig (X.Text text) = return $ ConfigNode (tryDecodeBS text) H.empty
xmlContentToConfig (X.CData cdata) = return $ ConfigNode (String $ decodeUtf8 cdata) H.empty

nodeConfig :: X.Node -> Reader Options ConfigNode
nodeConfig node = undefined
    --     let attrs = X.attributes node
    --         contents = skipWhitespace (X.contents node)
    --         attrC = foldr setAttr mempty attrs
    --     in foldr setContent attrC contents 
    -- where 
    --     setAttr (k, v) = set (B.unpack k) (tryDecodeBS v)
    --     setContent x c = xmlContentToConfig x <> c
    --     skipWhitespace = filter (not . isWhitespace)
    --     isWhitespace (X.Text text) = B.all isSpace text
    --     isWhitespace _ = False

splitAtEl :: Eq a => a -> [a] -> ([a], [a])
splitAtEl x xs = let (pre, rest) = span (/= x) xs
                 in (pre, if null rest then rest else tail rest)

tryDecodeS :: String -> Value
tryDecodeS = tryDecodeBS . B.pack

tryDecodeT :: T.Text -> Value
tryDecodeT = tryDecodeBS . encodeUtf8

tryDecodeBS :: B.ByteString -> Value
tryDecodeBS bs = fromMaybe (String $ decodeUtf8 bs) $ A.decodeStrict' bs

lcase :: String -> String
lcase = map toLower

mergeVal :: Value -> Value -> Value
mergeVal (Object o2) (Object o1) = Object $ H.unionWith mergeVal o2 o1
mergeVal Null v1 = v1
mergeVal v2 _ = v2

setC :: String -> ConfigNode -> ConfigNode -> ConfigNode
setC k c = swapC (const c) k

swapC :: (ConfigNode -> ConfigNode) -> String -> ConfigNode -> ConfigNode
swapC f "" c = f c
swapC f path (ConfigNode v m) = 
    let (key, rest) = splitAtEl '.' path
        c = swapC f rest mempty
    in ConfigNode v $ H.insertWith (<>) key c m

find :: String -> ConfigNode -> Maybe ConfigNode
find "" c = Just c
find p (ConfigNode _ m) = 
    let (k, rest) = splitAtEl '.' p
    in case H.lookup k m of
        Just c -> find rest c
        _ -> Nothing

makeGet :: (ConfigNode -> Either ConfigGetException a) -> String -> ConfigNode -> Either ConfigGetException a
makeGet f p = maybe (Left $ KeyNotFoundError p) f . find p

getE :: FromJSON a => String -> ConfigNode -> Either ConfigGetException a
getE = makeGet tryParse
    where
        tryParse (ConfigNode v _) = case A.fromJSON v of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

getM :: FromJSON a => String -> ConfigNode -> Maybe a
getM = get

get :: forall a m. (MonadThrow m, FromJSON a) => String -> ConfigNode -> m a
get p = either throwM return . getE p 

bindE:: FromJSON a => String -> ConfigNode -> Either ConfigGetException a
bindE = makeGet tryBind
    where
        tryBind c = case A.fromJSON $ A.toJSON c of
            A.Success a -> Right a
            A.Error e -> Left $ ParseValueError e

bindM :: FromConfig a => String -> ConfigNode -> Maybe a
bindM = bind

bind :: forall a m. (MonadThrow m, FromJSON a) => String -> ConfigNode -> m a
bind p = either throwM return . bindE p

swap :: (Value -> Value) -> String -> ConfigNode -> ConfigNode
swap f = let f' (ConfigNode v m) = ConfigNode (f v) m in swapC f'

set :: String -> Value -> ConfigNode -> ConfigNode
set k v = swap (const v) k