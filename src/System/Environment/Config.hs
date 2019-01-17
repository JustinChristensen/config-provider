{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module System.Environment.Config (
      Config
    , EnvReader
    , Value(..)
    , getConfig
    , jsonFileReader
    , xmlFileReader
    , yamlFileReader
    , iniFileReader
    , remoteReader
    , envReader
    , argsReader
    , getArgMap
    , getEnvMap
) where

import System.Directory (getPermissions, Permissions, readable)
import System.Environment (lookupEnv, getArgs, getEnvironment)
import Control.Exception (try, IOException)
import Control.Monad (when)
import Control.Applicative ((<|>))
import Control.Monad.State (StateT(..), execStateT, liftIO, get)
import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, stripPrefix)
import GHC.Generics
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Aeson.Types (typeMismatch)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Xeno.DOM as X
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Aeson as A
import qualified Data.Ini as I
import qualified Data.Yaml as YL
import qualified Data.HashMap.Strict as H

data Value = String !T.Text
           | Double !Double
           | Integer !Integer
           | Bool !Bool
           | Null
             deriving (Eq, Read, Show, Generic)

type Config = M.Map String Value
type EnvReader = StateT Config IO

instance A.FromJSON Value where
    parseJSON s@(A.String _) = return $ mapJsonVal s
    parseJSON n@(A.Number _) = return $ mapJsonVal n
    parseJSON b@(A.Bool _) = return $ mapJsonVal b
    parseJSON n@A.Null = return $ mapJsonVal n
    parseJSON invalid = typeMismatch "Value" invalid

newtype FlatValueMap = FlatValueMap { runFlatValueMap :: [(String, Value)] }
    deriving (Eq, Read, Show)

instance A.FromJSON FlatValueMap where
    parseJSON o@(A.Object _) = return $ FlatValueMap $ flatten "" (Right "") o []
    parseJSON a@(A.Array _) = return $ FlatValueMap $ flatten "" (Right "") a []
    parseJSON invalid = typeMismatch "[(String, Value)]" invalid

mapJsonVal :: A.Value -> Value
mapJsonVal (A.String s) = String s
mapJsonVal (A.Number n) = case floatingOrInteger n of
    Left r -> Double r
    Right i -> Integer i
mapJsonVal (A.Bool b) = Bool b
mapJsonVal A.Null = Null
mapJsonVal _ = error "use parseJSON to convert aeson arrays and objects"

vmToConfig :: FlatValueMap -> Config
vmToConfig = M.fromList . runFlatValueMap

flatten :: String -> Either Int T.Text -> A.Value -> [(String, Value)] -> [(String, Value)]
flatten path key val acc = case val of
        A.Object o -> H.foldrWithKey (flatten (dot path key) . Right) acc o
        A.Array a -> V.ifoldr (flatten (dot path key) . Left) acc a
        v -> (lcase $ dot path key, mapJsonVal v) : acc
    where 
        dot "" (Right k) = T.unpack k
        dot p (Left k) = p ++ '[' : show k ++ "]"
        dot p (Right k) = p ++ '.' : T.unpack k

normalizeKey :: String -> String
normalizeKey ('_':'_':cs) = '.' : normalizeKey (dropWhile (== '_') cs)
normalizeKey (c:cs) = toLower c : normalizeKey cs
normalizeKey [] = []

toVal :: String -> Value
toVal v = if null v then Bool True else String (T.pack v)

lcase :: String -> String
lcase = map toLower

filterEnv :: [String] -> [(String, String)] -> [(String, Value)]
filterEnv prefixes env = map envPair $ filter keyMatchesPrefix env
    where
        keyMatchesPrefix (k, _) = any ((`isPrefixOf` lcase k) . tailIfTilde . lcase) prefixes
        tailIfTilde ('~':cs) = cs
        tailIfTilde cs = cs
        envPair (k, v) = let    
                isLongestMatched lk ('~':p) Nothing  | lcase p `isPrefixOf` lk = Just p
                                                     | otherwise = Nothing
                isLongestMatched lk ('~':p) (Just l) | lcase p `isPrefixOf` lk && length p > length l = Just p
                                                     | otherwise = Just l
                isLongestMatched lk _ l = l
                stripLongestPrefix lk = fromMaybe lk $ 
                    foldr (isLongestMatched lk) Nothing prefixes >>= \p ->
                        stripPrefix (lcase p) lk
            in (normalizeKey $ stripLongestPrefix $ lcase k, toVal v)

argToPair :: String -> (String, Value)
argToPair ('-':'-':arg) = argToPair arg
argToPair arg = splitAtChar '=' arg
    where
        splitAtChar c str = let
                key = normalizeKey $ takeWhile (/= c) str
                val = case dropWhile (/= c) str of
                    "" -> toVal ""
                    cs -> toVal $ tail cs
            in (key, val)

mapArgs :: [String] -> [(String, Value)]
mapArgs (a1:a2:args) | wantsArg a1 && isArg a2 = argToPair (a1 ++ "=" ++ a2) : mapArgs args
                     | otherwise = argToPair a1 : mapArgs (a2:args)
    where wantsArg a = "--" `isPrefixOf` a && '=' `notElem` a
          isArg a = not ("--" `isPrefixOf` a) && '=' `notElem` a
mapArgs (arg:args) = argToPair arg : mapArgs args
mapArgs [] = []

parseVal :: String -> Value
parseVal v | lcase v == "true" = Bool True
           | lcase v == "false" = Bool False
           | otherwise = fromMaybe (String $ T.pack v) $ tryInteger v <|> tryDouble v
    where 
        tryInteger v = (readMaybe v :: Maybe Integer) >>= \v' -> return $ Integer v'
        tryDouble v = (readMaybe v :: Maybe Double) >>= \v' -> return $ Double v'

mapIniToConfig :: I.Ini -> [(String, Value)]
mapIniToConfig ini = let globals = toPair <$> I.iniGlobals ini
                     in H.foldrWithKey flattenIni globals $ I.iniSections ini
    where
        toPair (k, v) = (lcase $ T.unpack k, parseVal $ T.unpack v)
        dot p (k, v) = (T.concat [p, ".", k], v)
        flattenIni section pairs acc = foldr ((:) . toPair . dot section) acc pairs

mapXmlToConfig :: X.Node -> [(String, Value)]
mapXmlToConfig node = let attrPairs = flattenAttrs "" (X.attributes node) []
                      in flattenContents "" (X.contents node) attrPairs
    where 
        dot "" k = k
        dot p k = B.concat [p, ".", k]
        flattenNode path node acc = let
                path' = dot path (X.name node)
                attrPairs = flattenAttrs path' (X.attributes node) acc
            in flattenContents path' (X.contents node) attrPairs
        flattenAttrs path attrs acc = foldr (flattenAttr path) acc attrs
        flattenAttr path (k, v) acc = (lcase $ B.unpack $ dot path k, parseVal $ B.unpack v) : acc
        flattenContents path contents acc = foldr (flattenContent path) acc contents
        flattenContent path content acc = case content of
            X.Element node -> flattenNode path node acc
            X.Text text -> if B.all isSpace text then acc
                else flattenAttr "" (path, text) acc
            X.CData cdata -> flattenAttr "" (path, cdata) acc

getEnvMap :: [String] -> IO [(String, Value)]
getEnvMap prefixes = getEnvironment >>= return . filterEnv prefixes

getArgMap :: IO [(String, Value)]
getArgMap = getArgs >>= return . mapArgs

unifyConfig :: Config -> EnvReader ()
unifyConfig c2 = StateT $ \c1 ->
        return ((), M.unionWith pickVal c2 c1)
    where 
        pickVal Null v1 = v1
        pickVal v2 _ = v2

remoteReader :: (Config -> IO Config) -> EnvReader ()
remoteReader f = do
    c1 <- get
    c2 <- liftIO $ f c1
    unifyConfig c2

whenReadable :: FilePath -> (Config -> IO Config) -> EnvReader ()
whenReadable path f = do
    ePerms <- liftIO $ try $ getPermissions path
    case (ePerms :: Either IOException Permissions) of
        Right perms -> when (readable perms) $ remoteReader f
        _ -> return ()

jsonFileReader :: FilePath -> EnvReader ()
jsonFileReader path = whenReadable path $ \config -> do
    mValue <- A.decodeFileStrict' path 
    return $ maybe config vmToConfig (mValue :: Maybe FlatValueMap)

yamlFileReader :: FilePath -> EnvReader ()
yamlFileReader path = whenReadable path $ \config -> do
    eValue <- YL.decodeFileEither path
    return $ case (eValue :: Either YL.ParseException FlatValueMap) of 
        Right vm -> vmToConfig vm
        Left _ -> config -- for now we'll ignore exceptions, TODO: debate error handling API

xmlFileReader :: FilePath -> EnvReader ()
xmlFileReader path = whenReadable path $ \config -> do
    eNode <- B.readFile path >>= return . X.parse
    return $ case eNode of 
        Right node -> M.fromList $ mapXmlToConfig node
        Left _ -> config -- again, ignoring exceptions

iniFileReader :: FilePath -> EnvReader ()
iniFileReader path = whenReadable path $ \config -> do
    eIni <- I.readIniFile path
    return $ case eIni of
        Right ini -> M.fromList $ mapIniToConfig ini
        Left _ -> config -- again, ignoring exceptions

envReader :: [String] -> EnvReader ()
envReader prefixes = remoteReader $ \config -> do
    envMap <- getEnvMap prefixes
    return $ M.fromList envMap

argsReader :: EnvReader ()
argsReader = remoteReader $ \config -> do
    argMap <- getArgMap
    return $ M.fromList argMap

getConfig :: EnvReader () -> IO Config
getConfig reader = execStateT reader M.empty
