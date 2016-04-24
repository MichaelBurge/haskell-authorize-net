{-# LANGUAGE TemplateHaskell #-}

module Network.AuthorizeNet.TH where

import Control.Monad
import Data.Aeson.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Text.XML.HaXml hiding (Name)
import Text.XML.HaXml.Schema.Schema


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

requestOptions :: Options
requestOptions = choiceType {
  constructorTagModifier = \(x:xs) -> (toLower x : xs) ++ "Request"
  }

withType :: Name -> (Name -> [TyVarBndr] -> [Con] -> Q a) -> Q a
withType name f = do
  let ns = "Network.AuthorizeNet.TH.withType: "
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        DataD    _ _ tvbs cons _ -> f name tvbs cons
        NewtypeD _ _ tvbs con _  -> f name tvbs [con]
        other -> error $ ns ++ "Unsupported type: " ++ show other
    _ -> error $ ns ++ "Data constructor " ++ show name ++ " is not from a data or newtype constructor"

data SpecialType = SMaybe | SNone deriving (Show)

specialType :: Type -> SpecialType
specialType (AppT (ConT m) _) | m == ''Maybe = SMaybe
--specialType (AppT ListT _) = SList
specialType _ = SNone
  
deriveXmlSchemaType :: Options -> Name -> Q Dec
deriveXmlSchemaType opts name = withType name $ \name tvbs cons -> do
  let ns = "Network.AuthorizeNet.TH.deriveXmlSchemaType: Type - " ++ showName name ++ ": "
  let context = []
      ty = AppT (ConT ''SchemaType) (ConT name)
      con = case cons of
        [con] -> con
        _ -> error $ ns ++ "Expected exactly one constructor on type " ++ showName name
  (Just conName) <- lookupValueName $ constructorTagModifier opts $ case con of
        RecC name _ -> showName name
        _ -> error $ ns ++ "conName - unsupported constructor"
  let parseOneField :: Exp -> VarStrictType -> Q Exp
      parseOneField previous (fieldNameRaw, _, ty) = do
        let fieldName = showName fieldNameRaw :: String
        let parseExpr = [| parseSchemaType $(return $ LitE $ StringL fieldName) |]
        currentNode <- case specialType ty of
          SMaybe -> [| optional $(parseExpr) |]
--          SList  -> [| $(return $ ConE $ mkName "ArrayOf") <$> many $(parseExpr) |]
          SNone  -> parseExpr
        return $ InfixE (Just previous) (VarE $ mkName "apply") $ Just currentNode
      parseConstructor :: Con -> ExpQ
      parseConstructor (RecC name vsts) = foldM parseOneField (AppE (VarE $ mkName "return") (ConE name)) vsts
      parseConstructor _ = error $ ns ++ "Unsupported constructor for type"
      decParseSchemaType :: DecsQ
      decParseSchemaType = do
        body <- [| do
                    (pos,e) <- posnElement [$(return $ VarE $ mkName "s")]
                    commit $ interior e $ $(parseConstructor con)
                 |]
        return $ pure $ FunD (mkName "parseSchemaType") [Clause [VarP $ mkName "s"] (NormalB body) []]
      toXmlOneField :: VarStrictType -> ExpQ
      toXmlOneField (fieldName, _, ty) =
        let xmlName = fieldLabelModifier opts $ showName fieldName
        in [| schemaTypeToXML $(return $ LitE $ StringL xmlName) $(return $ AppE (VarE fieldName) (VarE $ mkName "x")) |]
      decSchemaTypeToXml :: DecsQ
      decSchemaTypeToXml =
        let vsts = case con of
              NormalC{} -> error $ ns ++ "You must use record syntax when automatically deriving SchemaType instances"
              RecC _ vsts -> vsts
              _ -> error $ ns ++ "Unsupported constructor for type"
        in do
          exps <- mapM toXmlOneField vsts
          body <- [| toXMLElement $(return $ VarE $ mkName "s") [] $(return $ ListE exps) |]
          let clause = Clause [VarP $ mkName "s", AsP (mkName "x") $ RecP conName []] (NormalB body) []
          return $ pure $ FunD (mkName "schemaTypeToXML") [clause]
  decs <- (++) <$> decParseSchemaType <*> decSchemaTypeToXml
  return $ InstanceD context ty decs

deriveXml :: Options -> Name -> Q [Dec]
deriveXml opts name = pure <$> deriveXmlSchemaType opts name
               
