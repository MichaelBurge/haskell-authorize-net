module Network.AuthorizeNet.TH where

import Data.Aeson.TH

import Data.Char
import qualified Data.Text as T

dropUntilUnderscore :: String -> String
dropUntilUnderscore name = tail $ dropWhile (/= '_') name

myDefaultOptions = defaultOptions { omitNothingFields = True }

-- | Drops everything up to and including the first underscore, so 'recordType_fieldOne' becomes 'fieldOne'
dropRecordName :: Options
dropRecordName = myDefaultOptions {
  fieldLabelModifier = dropUntilUnderscore
  }


choiceType :: Options
choiceType = myDefaultOptions {
  fieldLabelModifier = dropUntilUnderscore,
  constructorTagModifier = dropUntilUnderscore,
  allNullaryToStringTag = True,
  sumEncoding = ObjectWithSingleField
  }

enumType :: Options
enumType = choiceType
