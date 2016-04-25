{-# LANGUAGE ScopedTypeVariables #-}

module Network.AuthorizeNet.Util where

import Text.XML.HaXml
import Text.XML.HaXml.Parse hiding (document)
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Schema.Schema
import Text.XML.HaXml.Types

import Text.ParserCombinators.Poly.Plain

import Network.AuthorizeNet.Instances
import Network.AuthorizeNet.TH

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

runXmlParser :: String -> XMLParser a -> BSL.ByteString -> Either String a
runXmlParser filename parser bsl =
  let string = TL.unpack $ TL.decodeUtf8 bsl
      eDocument = xmlParse' filename string
      eElement = (\doc -> case doc of Document _ _ element _ -> element) <$> eDocument
  in fst . runParser parser . (\element -> [ CElem (element) noPos ]) =<< eElement

runParseSchemaType :: forall a. XmlParsable a => BSL.ByteString -> Either String a
runParseSchemaType bsl = do
  result <- runXmlParser "Authorize.NET API" (parseSchemaType $ xmlParsableName (undefined :: a)) bsl
  return $ (result :: a)

runSchemaTypeToXml :: forall a. XmlParsable a => a -> BSL.ByteString
runSchemaTypeToXml x =
  let name = xmlParsableName x
      cons = children $ head $ schemaTypeToXML name x
      nsAttr = AttValue [Left "AnetApi/xml/v1/schema/AnetApiSchema.xsd"]
      xmlDecl = XMLDecl "1.0" (Just $ EncodingDecl "utf-8") Nothing
      doc = document $ Document (Prolog (Just xmlDecl) [] Nothing []) [] (Elem (N name) [(N "xmlns", nsAttr)] cons ) []
  in TL.encodeUtf8 $ TL.pack $ render doc

fromXml :: forall a. XmlParsable a => BSL.ByteString -> Either String a
fromXml = runParseSchemaType
toXml :: forall a. XmlParsable a => a -> BSL.ByteString
toXml = runSchemaTypeToXml
