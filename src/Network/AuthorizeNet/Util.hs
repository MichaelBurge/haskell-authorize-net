{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module Network.AuthorizeNet.Util where

import Text.XML.HaXml
import Text.XML.HaXml.Parse hiding (document)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Schema.Schema
import Text.XML.HaXml.Types

import Text.ParserCombinators.Poly.Plain

import Network.AuthorizeNet.Types

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

data XmlParseOptions = XmlParseOptions {
  xmlParseOption_overrideElementName :: Maybe String
  } deriving (Eq, Show)

defaultOptions :: XmlParseOptions
defaultOptions = XmlParseOptions Nothing

runXmlParser :: String -> XMLParser a -> BSL.ByteString -> Either String a
runXmlParser filename parser bsl =
  let string = TL.unpack $ TL.decodeUtf8 bsl
      eDocument = xmlParse' filename string
      eElement = (\doc -> case doc of Document _ _ element _ -> element) <$> eDocument
  in fst . runParser parser . (\element -> [ CElem (element) noPos ]) =<< eElement

runParseSchemaTypeWithOptions :: forall a. XmlParsable a => XmlParseOptions -> BSL.ByteString -> Either String a
runParseSchemaTypeWithOptions options bsl = do
  let xmlName = case xmlParseOption_overrideElementName options of
        Nothing -> xmlParsableName (undefined :: a)
        Just x -> x
  result <- runXmlParser "Authorize.NET API" (parseSchemaType xmlName) bsl
  return $ (result :: a)


runParseSchemaType :: forall a. XmlParsable a => BSL.ByteString -> Either String a
runParseSchemaType bsl = runParseSchemaTypeWithOptions defaultOptions bsl

runSchemaTypeToXmlWithOptions :: forall a. XmlParsable a => XmlParseOptions -> a -> BSL.ByteString
runSchemaTypeToXmlWithOptions options x = 
  let name = case xmlParseOption_overrideElementName options of
        Nothing -> xmlParsableName x
        Just y -> y
      cons = children $ head $ schemaTypeToXML name x
      nsAttrs Namespace_none = []
      nsAttrs Namespace_xsd = [(N "xmlns", AttValue [Left "AnetApi/xml/v1/schema/AnetApiSchema.xsd"])]
      nsAttrs Namespace_full = [
        (N "xmlns:xsi", AttValue [Left "http://www.w3.org/2001/XMLSchema-instance"]),
        (N "xmlns:xsd", AttValue [Left "http://www.w3.org/2001/XMLSchema"]),
        (N "xmlns", AttValue [Left "AnetApi/xml/v1/schema/AnetApiSchema.xsd"])
        ]
      xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "utf-8") Nothing
      doc = document $ Document (Prolog (Just xmlDecl) [] Nothing []) [] (Elem (N name) (nsAttrs $ xmlNamespaceLevel x) cons ) []
  in TL.encodeUtf8 $ TL.pack $ render doc

                                 
runSchemaTypeToXml :: forall a. XmlParsable a => a -> BSL.ByteString
runSchemaTypeToXml x = runSchemaTypeToXmlWithOptions defaultOptions x

fromXml :: forall a. XmlParsable a => BSL.ByteString -> Either String a
fromXml = runParseSchemaType

toXml :: forall a. XmlParsable a => a -> BSL.ByteString
toXml = runSchemaTypeToXml
