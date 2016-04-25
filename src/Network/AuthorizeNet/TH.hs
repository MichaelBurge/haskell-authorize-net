{-# LANGUAGE TemplateHaskell #-}

module Network.AuthorizeNet.TH (
  module Network.AuthorizeNet.TH,
  apply,
  parseSchemaType,
  schemaTypeToXML
  ) where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Network.AuthorizeNet.Instances

import Text.XML.HaXml hiding (Name, element, literal)
import Text.XML.HaXml.Schema.Schema

import System.IO

import Data.Char
import Data.List.Split as L (splitOn)
import qualified Data.Text as T

data Options = Options {
  fieldLabelModifier     :: String -> String,
  constructorTagModifier :: String -> String,
  typeTagModifier        :: String -> String,
  allNullaryToStringTag  :: Bool
  }

dropUntilUnderscore :: String -> String
dropUntilUnderscore name =
  case dropWhile (/= '_') name of
    [] -> error $ "Network.AuthorizeNet.TH.dropUntilUnderscore: When processing '" ++ name ++ "': No underscore in name or no text after underscore"
    xs -> tail xs

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = (toLower x):xs

dropHaskellModuleNames :: String -> String
dropHaskellModuleNames xs = last $ L.splitOn "." xs

defaultOptions :: Options
defaultOptions = Options {
  fieldLabelModifier     = dropUntilUnderscore,
  constructorTagModifier = dropUntilUnderscore,
  typeTagModifier        = lowerFirst . dropHaskellModuleNames,
  allNullaryToStringTag  = True
  }

-- | Drops everything up to and including the first underscore, so 'recordType_fieldOne' becomes 'fieldOne'
dropRecordName :: Options
dropRecordName = defaultOptions


choiceType :: Options
choiceType = defaultOptions

enumType :: Options
enumType = defaultOptions

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

type XmlName = String
type ConName = String

-- | Fills out Restricts, SchemaType, and SimpleType for a simple NewType wrapper
deriveXmlNewtype :: Options -> Name -> DecsQ
deriveXmlNewtype opts name = do
  wrappedTypeName <- conName <$> getWrappedTypeCon name
  let outerType = return $ ConT name
      innerType = return $ ConT wrappedTypeName
      outerPattern = return $ ConP name [VarP $ mkName "x"]
      outerCon = return $ ConE name
      restrictsDec = [d|
        instance Restricts $(outerType) $(innerType) where
          restricts $(outerPattern) = x
        |]
      schemaTypeDec = [d|
        instance SchemaType $(outerType) where
          parseSchemaType s = do
            e <- element [s]
            commit $ interior e $ parseSimpleType
          schemaTypeToXML s $(outerPattern) =
            toXMLElement s [] [toXMLText (simpleTypeText x)]
        |]

      simpleTypeDec = [d|
        instance SimpleType $(outerType) where
          acceptingParser = fmap $(outerCon) acceptingParser
          simpleTypeText $(outerPattern) = simpleTypeText x
        |]
  
  concat <$> sequence [restrictsDec, schemaTypeDec, simpleTypeDec]

joinExprs :: [ExpQ] -> ExpQ -> ExpQ
joinExprs leaves joiner = do
  j <- joiner
  (x:xs) <- sequence leaves
  return $ foldl (\p a -> InfixE (Just p) j (Just a)) x xs

-- | data X = A | B | C is just a simple enum, so it gets treated as strings
deriveXmlEnum :: Options -> Name -> DecsQ
deriveXmlEnum opts name = withType name $ \name tvbs cons -> do
  let doExpr :: (XmlName, ConName) -> Q Exp
      doExpr (xmlName, conName) = [| do literal $(return $ LitE $ StringL xmlName) ; return $(return $ ConE $ mkName conName) |]
      conInfo :: Con -> (XmlName, ConName)
      conInfo con = let cn = showName $ conName con in (constructorTagModifier opts cn, cn)
      nameInfos :: [(XmlName, ConName)]
      nameInfos = map conInfo cons
      acceptingParserBody :: ExpQ
      acceptingParserBody = joinExprs (map doExpr nameInfos) (return $ VarE $ mkName "onFail")
      nameInfoClause :: (XmlName, ConName) -> Clause
      nameInfoClause (xmlName, conName) = Clause [ConP (mkName conName) []] (NormalB $ LitE $ StringL xmlName) []
      simpleTypeTextDec :: Dec
      simpleTypeTextDec = FunD (mkName "simpleTypeText") (map nameInfoClause nameInfos)
      simpleTypeInstanceDec :: Q Dec
      simpleTypeInstanceDec = do
        acceptingParserB <- acceptingParserBody
        let acceptingParserDec = FunD (mkName "acceptingParser") [Clause [] (NormalB $ acceptingParserB) []]
        return $ InstanceD [] (AppT (ConT ''SimpleType) $ ConT name) [
          acceptingParserDec,
          simpleTypeTextDec
          ]
  
      schemaTypeInstanceDec = [d|
        instance SchemaType $(return $ ConT name) where
          parseSchemaType s = do
            e <- element [s]
            commit $ interior e $ parseSimpleType
          schemaTypeToXML s x =
            toXMLElement s [] [toXMLText (simpleTypeText x)]
        |]
  (++) <$> schemaTypeInstanceDec <*> (pure <$> simpleTypeInstanceDec)

deriveXmlObject :: Options -> Name -> DecsQ
deriveXmlObject opts name = withType name $ \name tvbs cons -> do
  let ns = "Network.AuthorizeNet.TH.deriveXmlSchemaType: Type - " ++ showName name ++ ": "
  let context = []
      ty = AppT (ConT ''SchemaType) (ConT name)
      con = case cons of
        [con] -> con
        _ -> error $ ns ++ "Expected exactly one constructor on type " ++ showName name
      conN = conName con
      xV = VarE $ mkName "x"
      sV = VarE $ mkName "s"
  let parseOneField :: Exp -> VarStrictType -> Q Exp
      parseOneField previous (fieldNameRaw, _, ty) = do
        let fieldName = showName fieldNameRaw :: String
            xmlName = fieldLabelModifier opts fieldName
        let parseExpr = [| parseSchemaType $(return $ LitE $ StringL xmlName) |]
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
                    (pos,e) <- posnElement [$(return sV)]
                    commit $ interior e $ $(parseConstructor con)
                 |]
        return $ pure $ FunD (mkName "parseSchemaType") [Clause [VarP $ mkName "s"] (NormalB body) []]
      toXmlOneField :: VarStrictType -> ExpQ
      toXmlOneField (fieldName, _, ty) =
        let xmlName = fieldLabelModifier opts $ showName fieldName
            sttxE = [| schemaTypeToXML $(return $ LitE $ StringL xmlName) |]
        in case specialType ty of
          SMaybe -> [| maybe [] $(sttxE) $ $(return $ AppE (VarE fieldName) xV) |]
          SNone -> [| $(sttxE) $ $(return $ AppE (VarE fieldName) xV) |]
      decSchemaTypeToXml :: DecsQ
      decSchemaTypeToXml =
        let vsts = case con of
              NormalC{} -> error $ ns ++ "You must use record syntax when automatically deriving SchemaType instances"
              RecC _ vsts -> vsts
              _ -> error $ ns ++ "Unsupported constructor for type"
        in do
          exps <- mapM toXmlOneField vsts
          body <- [| toXMLElement $(return $ VarE $ mkName "s") [] $(return $ ListE exps) |]
          let clause = Clause [VarP $ mkName "s", AsP (mkName "x") $ RecP conN []] (NormalB body) []
          return $ pure $ FunD (mkName "schemaTypeToXML") [clause]
  decs <- (++) <$> decParseSchemaType <*> decSchemaTypeToXml
  return $ [ InstanceD context ty decs ]

conName :: Con -> Name
conName con = case con of
  NormalC x _ -> x
  RecC x _ -> x
  _ -> error $ "Network.AuthorizeNet.TH.conName: Unsupported constructor type" ++ show con

isAllNullary :: [Con] -> Bool
isAllNullary cons = flip all cons $ \con ->
  case con of
    NormalC _ [] -> True
    NormalC _ _ -> False
    RecC _ [] -> True
    RecC _ _ ->  False
    _ -> error $ "Network.AuthorizeNet.TH.isAllNullary: Unsupported constructor type " ++ show cons

getWrappedTypeCon :: Name -> Q Con
getWrappedTypeCon name = do
  let ns = "Network.AuthorizeNet.TH.getWrappedTypeName: Name was " ++ showName name ++ ": "
  info <- reify name
  case info of
    TyConI dec ->
      case dec of
        NewtypeD _ _ _ con _ -> return con
        _ -> error $ ns ++ "Unexpected declaration"

deriveXmlParsable :: Options -> Name -> Q [Dec]
deriveXmlParsable opts name =
  [d|
   instance XmlParsable $(return $ ConT name) where
     xmlParsableName _ = $(return $ LitE $ StringL $ typeTagModifier opts $ showName name)
  |]

-- | The main intended entry point
deriveXml :: Options -> Name -> Q [Dec]
deriveXml opts name = do
  let ns = "Network.AuthorizeNet.TH.deriveXml: Name was '" ++ showName name ++ "':"
  info <- reify name
  decs <- case info of
    TyConI dec ->
      case dec of
        NewtypeD{} -> deriveXmlNewtype opts name
        DataD _ _ tvbs cons _ -> if isAllNullary cons && allNullaryToStringTag opts
                                 then deriveXmlEnum opts name
                                 else deriveXmlObject opts name
        _ -> error $ ns ++ "Only newtypes or data constructors can be derived."
  xmlParsableDecs <- deriveXmlParsable opts name
  return $ decs ++ xmlParsableDecs

               
