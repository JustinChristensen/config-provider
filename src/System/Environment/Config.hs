{-# LANGUAGE OverloadedStrings #-}
module System.Environment.Config (
      module System.Environment.Config.Types
    , jsonFileE
    , jsonFile
    , optionalJsonFile
    , jsonFileReader
    , optionalJsonFileReader
    , yamlFileE
    , yamlFile
    , optionalYamlFile
    , yamlFileReader
    , optionalYamlFileReader
    , xmlFileE
    , xmlFile
    , optionalXmlFile
    , xmlFileReader
    , optionalXmlFileReader
    , iniFileE
    , iniFile
    , optionalIniFile
    , iniFileReader
    , optionalIniFileReader
    , envReader
    , argsReader
    , remoteReader
    , getConfig
) where

import System.Environment.Config.Reader
import System.Environment.Config.Types