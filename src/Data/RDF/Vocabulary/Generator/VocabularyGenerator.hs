{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RDF.Vocabulary.Generator.VocabularyGenerator ( genVocabulary ) where

import           Data.Char           (isLower)
import           Data.List           (nub)
import           Data.Maybe          (maybeToList)
import           Data.RDF            (AdjHashMap, Node, Node (UNode), RDF, Rdf,
                                      TurtleParser (TurtleParser), parseFile,
                                      subjectOf, triplesOf)
import           Data.Text           (Text, cons, split, unpack)
import qualified Data.Text           as T
import           Language.Haskell.TH

genVocabulary :: String -> Q [Dec]
genVocabulary file = vocabulary <$> runIO (loadGraph file)

loadGraph :: String -> IO (RDF AdjHashMap)
loadGraph file = parseFile (TurtleParser Nothing Nothing) file >>= \result -> case result of
    Left  err      -> error $ show err
    Right rdfGraph -> return rdfGraph

vocabulary :: Rdf a => RDF a -> [Dec]
vocabulary graph =
  let subjects = nub $ subjectOf <$> triplesOf graph
      iris     = extractIRIs subjects
      names    = iriToName <$> iris
      iriDecls = zipWith declareIRI names iris
      irisDecl = declareIRIs names
  in  irisDecl : iriDecls

extractIRIs :: [Node] -> [Text]
extractIRIs nodes = concat $ maybeToList . toIRI <$> nodes

toIRI :: Node -> Maybe Text
toIRI (UNode iri) = Just iri
toIRI _           = Nothing

declareIRI :: Name -> Text -> Dec
declareIRI name iri =
  let iriLiteral = LitE . StringL $ unpack iri
  in  FunD name [Clause [] (NormalB iriLiteral) []]

declareIRIs :: [Name] -> Dec
declareIRIs names =
  let iriList = ListE (VarE <$> names)
  in  FunD (mkName "iris") [Clause [] (NormalB iriList) []]

iriToName :: Text -> Name
iriToName = mkName . unpack . escape . last . filter (not . T.null) . split (`elem` separators)
  where separators = ['/','#']

escape :: Text -> Text
escape name = escapeKeywords $ T.map escapeOperators name
  where escapeOperators c | c `elem` operators = escapeChar
        escapeOperators c = c
        escapeKeywords name | not (isLower $ T.head name) = escapeChar `cons` name
        escapeKeywords name | name `elem` keywords        = escapeChar `cons` name
        escapeKeywords name = name
        operators = ['!','#','$','%','&','*','+','.','/','<','=','>','?','@','\\','^','|','-','~']
        keywords = ["as", "case", "of", "class", "data", "data family", "data instance",
            "default", "deriving", "deriving instance", "do", "forall", "foreign",
            "hiding", "if", "then", "else", "import", "infix", "infixl", "infixr",
            "instance", "let", "in", "mdo", "module", "newtype", "proc", "qualified",
            "rec", "type", "type family", "type instance", "where"]
        escapeChar = '_'
