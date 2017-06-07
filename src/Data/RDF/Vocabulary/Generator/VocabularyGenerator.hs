{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.RDF.Vocabulary.Generator.VocabularyGenerator (
  genVocabulary
) where

import           ClassyPrelude
import           Data.Char           (isLower)
import           Data.Maybe          (maybeToList)
import           Data.RDF            (AdjHashMap, Node (UNode), RDF, Rdf,
                                      TurtleParser (TurtleParser), parseFile,
                                      subjectOf, triplesOf, prefixMappings, PrefixMappings(PrefixMappings))
import qualified Data.Text           as T
import qualified Data.Map as M
import           Language.Haskell.TH

genVocabulary :: String -> Q [Dec]
genVocabulary file = vocabulary <$> runIO (loadGraph file)

loadGraph :: String -> IO (RDF AdjHashMap)
loadGraph file = parseFile (TurtleParser Nothing Nothing) file >>= \result -> case result of
    Left  err      -> error $ show err
    Right rdfGraph -> return rdfGraph

vocabulary :: Rdf a => RDF a -> [Dec]
vocabulary graph =
  let nameDecls = do
        subject <- hashNub $ subjectOf <$> triplesOf graph
        iri <- maybeToList $ toIRI subject
        name <- maybeToList $ iriToName iri
        return (name, declareIRI name iri)
      (PrefixMappings prefixMappings') = prefixMappings graph
      namespaceDecls = do
        (prefix, iri) <- M.toList prefixMappings'
        let name = mkName . unpack . escape $ prefix <> "NS"
        return $ declarePrefix name prefix iri
      iriDecls = snd <$> nameDecls
      irisDecl = declareIRIs $ fst <$> nameDecls
  in irisDecl : namespaceDecls <> iriDecls

toIRI :: Node -> Maybe Text
toIRI (UNode iri) = Just iri
toIRI _           = Nothing

packFun :: Exp
packFun = VarE $ mkName "Data.Text.pack"

unodeFun :: Exp
unodeFun = VarE $ mkName "Data.RDF.Types.unode"

mkPrefixedNSFun :: Exp
mkPrefixedNSFun = VarE $ mkName "Data.RDF.Namespace.mkPrefixedNS"

declareIRI :: Name -> Text -> Dec
declareIRI name iri =
  let iriLiteral = LitE . StringL $ unpack iri
      unodeLiteral = AppE unodeFun $ AppE packFun iriLiteral
  in  FunD name [Clause [] (NormalB unodeLiteral) []]

declareIRIs :: [Name] -> Dec
declareIRIs names =
  let iriList = ListE (VarE <$> names)
  in  FunD (mkName "iris") [Clause [] (NormalB iriList) []]


-- namespace = mkPrefixedNS "ogit" "http://www.purl.org/ogit/"
declarePrefix :: Name -> Text -> Text -> Dec
declarePrefix name prefix iri =
  let prefixLiteral = AppE packFun . LitE . StringL . unpack $ prefix
      iriLiteral = AppE packFun . LitE . StringL . unpack $ iri
      namespace = AppE (AppE mkPrefixedNSFun prefixLiteral) iriLiteral
  in  FunD name [Clause [] (NormalB namespace) []]

iriToName :: Text -> Maybe Name
iriToName iri = mkName . unpack . escape <$> (lastMay . filter (not . T.null) . T.split (`elem` separators)) iri
  where separators = ['/','#']

escape :: Text -> Text
escape name = escapeKeywords $ T.map escapeOperators name
  where escapeOperators c | c `elem` operators = escapeChar
        escapeOperators c = c
        escapeKeywords name' | not (isLower $ T.head name') = escapeChar `cons` name'
        escapeKeywords name' | name' `elem` keywords        = escapeChar `cons` name'
        escapeKeywords name' = name'
        operators = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~']
        keywords = ["as", "case", "of", "class", "data", "data family", "data instance",
            "default", "deriving", "deriving instance", "do", "forall", "foreign",
            "hiding", "if", "then", "else", "import", "infix", "infixl", "infixr",
            "instance", "let", "in", "mdo", "module", "newtype", "proc", "qualified",
            "rec", "type", "type family", "type instance", "where"]
        escapeChar = '_'
