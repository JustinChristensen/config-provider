module System.Environment.Config.ReaderSpec (spec) where

import Test.Hspec
-- import System.Environment.Config.Reader

spec :: Spec
spec = do
    describe "normalizeKey" $ do
        it "replaces two or more underscores with a period" pending
        it "lowercases the key" pending
    describe "filterEnv" $ do
        it "matches and strips the longest tilde prefix from the key" pending
        it "matches and preserves non-tilde prefixes in the key" pending
        it "omits environment variables that do not match the prefixes" pending
    describe "argToPair" $ do
        it "strips leading dashes from the key" pending
        it "splits the key and the value on '='" pending
        it "normalizes the key" pending
        it "attempts to parse the value" pending
    describe "mapArgs" $ do
        it "creates an associative array from command line args" pending
        it "skips two dashes" pending
        it "handles --foo=bar" pending
        it "handles foo=bar" pending
        it "handles --foo" pending
        it "handles --foo bar" pending
        it "handles --foo --bar" pending
        it "handles --foo --bar=baz" pending
    describe "getEnvPairs" $ do
        it "respects the provided prefixes" pending
        it "creates env pairs from environment variables" pending
    describe "getArgPairs" $ 
        it "creates arg pairs from command line arguments" pending
    describe "fromSource" $ do
        it "converts a value of the source type to the desired type" pending
        it "returns an exception if the conversion fails" pending
    describe "makeThrow" $ 
        it "lifts the provided file configuration source to MonadThrow" pending
    describe "makeOptional" $
        it "ignores the exception if the file does not exist" pending
    describe "makeEnvReader" $ do
        it "makes a new reader from the provided action" pending
        it "merges the result with the previous configuration" pending
    describe "remoteReader" $ do
        it "makes a new reader from the provided action" pending
        it "merges the result with the previous configuration" pending
    describe "jsonFileE" $ do
        it "reads a json file and returns the converted configuration" pending
        it "reads a json file and returns an exception if the file is missing" pending
        it "reads a json file and returns an exception if the conversion fails" pending
    describe "jsonFile" $ do
        it "reads a json file and returns the converted configuration" pending
        it "reads a json file and throws an exception if the file is missing" pending
        it "reads a json file and throws an exception if the conversion fails" pending
    describe "optionalJsonFile" $ do
        it "reads a json file and returns the converted configuration" pending
        it "reads a json file and returns the previous configuration if the file is missing" pending
        it "reads a json file and throws an exception if the conversion fails" pending
    describe "jsonFileReader" $ do
        it "reads a json file and merges the next configuration with the previous" pending
        it "reads a json file and throws an exception if the file is missing" pending
        it "reads a json file and throws an exception if the conversion fails" pending
    describe "optionalJsonFileReader" $ do
        it "reads a json file and merges the next configuration with the previous" pending
        it "reads a json file and maintains the previous configuration if the file is missing" pending
        it "reads a json file and throws an exception if the conversion fails" pending
    describe "yamlFileE" $ do
        it "reads a yaml file and returns the converted configuration" pending
        it "reads a yaml file and returns an exception if the file is missing" pending
        it "reads a yaml file and returns an exception if the conversion fails" pending
    describe "yamlFile" $ do
        it "reads a yaml file and returns the converted configuration" pending
        it "reads a yaml file and throws an exception if the file is missing" pending
        it "reads a yaml file and throws an exception if the conversion fails" pending
    describe "optionalYamlFile" $ do
        it "reads a yaml file and returns the converted configuration" pending
        it "reads a yaml file and returns the previous configuration if the file is missing" pending
        it "reads a yaml file and throws an exception if the conversion fails" pending
    describe "yamlFileReader" $ do
        it "reads a yaml file and merges the next configuration with the previous" pending
        it "reads a yaml file and throws an exception if the file is missing" pending
        it "reads a yaml file and throws an exception if the conversion fails" pending
    describe "optionalYamlFileReader" $ do
        it "reads a yaml file and merges the next configuration with the previous" pending
        it "reads a yaml file and maintains the previous configuration if the file is missing" pending
        it "reads a yaml file and throws an exception if the conversion fails" pending
    describe "xmlFileE" $ do
        it "skips the root node name during the conversion" pending
        it "reads a xml file and returns the converted configuration" pending
        it "reads a xml file and returns an exception if the file is missing" pending
        it "reads a xml file and returns an exception if the conversion fails" pending
    describe "xmlFile" $ do
        it "skips the root node name during the conversion" pending
        it "reads a xml file and returns the converted configuration" pending
        it "reads a xml file and throws an exception if the file is missing" pending
        it "reads a xml file and throws an exception if the conversion fails" pending
    describe "optionalXmlFile" $ do
        it "skips the root node name during the conversion" pending
        it "reads a xml file and returns the converted configuration" pending
        it "reads a xml file and returns the previous configuration if the file is missing" pending
        it "reads a xml file and throws an exception if the conversion fails" pending
    describe "xmlFileReader" $ do
        it "skips the root node name during the conversion" pending
        it "reads a xml file and merges the next configuration with the previous" pending
        it "reads a xml file and throws an exception if the file is missing" pending
        it "reads a xml file and throws an exception if the conversion fails" pending
    describe "optionalXmlFileReader" $ do
        it "skips the root node name during the conversion" pending
        it "reads a xml file and merges the next configuration with the previous" pending
        it "reads a xml file and maintains the previous configuration if the file is missing" pending
        it "reads a xml file and throws an exception if the conversion fails" pending
    describe "iniFileE" $ do
        it "reads a ini file and returns the converted configuration" pending
        it "reads a ini file and returns an exception if the file is missing" pending
        it "reads a ini file and returns an exception if the conversion fails" pending
    describe "iniFile" $ do
        it "reads a ini file and returns the converted configuration" pending
        it "reads a ini file and throws an exception if the file is missing" pending
        it "reads a ini file and throws an exception if the conversion fails" pending
    describe "optionalIniFile" $ do
        it "reads a ini file and returns the converted configuration" pending
        it "reads a ini file and returns the previous configuration if the file is missing" pending
        it "reads a ini file and throws an exception if the conversion fails" pending
    describe "iniFileReader" $ do
        it "reads a ini file and merges the next configuration with the previous" pending
        it "reads a ini file and throws an exception if the file is missing" pending
        it "reads a ini file and throws an exception if the conversion fails" pending
    describe "optionalIniFileReader" $ do
        it "reads a ini file and merges the next configuration with the previous" pending
        it "reads a ini file and maintains the previous configuration if the file is missing" pending
        it "reads a ini file and throws an exception if the conversion fails" pending
    describe "envReader" $ do
        it "parses environment variables and merges them with the previous configuration" pending
        it "respects the provided prefixes" pending
        it "throws an exception if parsing fails" pending
    describe "argsReader" $ do
        it "parses command line arguments and merges them with the previous configuration" pending
        it "throws an exception if parsing fails" pending
    describe "getConfig" $ 
        it "executes the provided environment reader and returns the result" pending