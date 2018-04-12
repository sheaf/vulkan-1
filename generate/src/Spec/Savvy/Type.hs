{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Spec.Savvy.Type
  ( Type(..)
  , ArraySize(..)
  , TypeParseContext
  , stringToType
  , stringToTypeExpected
  , specParserContext
  , typeDepends
  ) where

import           Control.Monad.Trans.Reader               (ReaderT (..))
import           Data.Either.Validation
import qualified Data.HashSet                             as HashSet
import           Data.Maybe
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           Data.Traversable
import qualified Language.C.Types                         as C
import           Spec.Savvy.Error
import qualified Spec.Spec                                as P
import qualified Spec.Type                                as P
import qualified Text.ParserCombinators.Parsec.Combinator as Parsec
import           Write.Element


data Type
  = Float
  | Void
  | Char
  | Int
    -- ^ Signed int
  | Ptr Type
  | Array ArraySize Type
  | TypeName Text
  | Proto Type [(Maybe Text, Type)]
  deriving (Show)

data ArraySize
  = NumericArraySize Word
  | SymbolicArraySize Text
  deriving (Show)

type CType = C.Type C.CIdentifier

type TypeParseContext = C.CParserContext C.CIdentifier

stringToTypeExpected
  :: TypeParseContext
  -- ^ The collection of all the type names
  -> Text
  -- ^ The expected id
  -> Text
  -- ^ The string to parse
  -> Either [SpecError] Type
stringToTypeExpected pc expected string = do
  (cId, t) <- stringToType pc string
  case cId of
    Just cId' | cId' /= expected -> Left [MismatchTypeName cId' expected]
    _         -> pure ()
  pure t

stringToType :: TypeParseContext -> Text -> Either [SpecError] (Maybe Text, Type)
stringToType pc string = do
  C.ParameterDeclaration cId cType <- stringToCType pc string
  t <- cTypeToType cType
  pure (T.pack . C.unCIdentifier <$> cId, t)

cTypeToType :: CType -> Either [SpecError] Type
cTypeToType = \case
  C.TypeSpecifier (C.Specifiers []          []        []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers []          [C.CONST] []) t -> nameToType t
  C.TypeSpecifier (C.Specifiers [C.TYPEDEF] []        []) t -> nameToType t
  C.Ptr           []        t  -> Ptr <$> cTypeToType t
  C.Ptr           [C.CONST] t  -> Ptr <$> cTypeToType t
  C.Array         s         t  -> Array <$> arraySize s <*> cTypeToType t
  C.Proto ret ps -> Proto <$> cTypeToType ret <*> traverse cParamDeclToType ps
  c                            -> Left [UnhandledCType (showText c)]

cParamDeclToType
  :: C.ParameterDeclaration C.CIdentifier
  -> Either [SpecError] (Maybe Text, Type)
cParamDeclToType (C.ParameterDeclaration cId t) =
  (,) (T.pack . C.unCIdentifier <$> cId) <$> cTypeToType t

arraySize :: C.ArrayType C.CIdentifier -> Either [SpecError] ArraySize
arraySize = \case
  C.SizedByInteger i
    | i >= 0
    -> pure $ NumericArraySize (fromInteger i)
    | otherwise
    -> Left [NegativeCArraySize i]
  C.SizedByIdentifier t -> pure $ SymbolicArraySize (T.pack (C.unCIdentifier t))
  s -> Left [UnhandledCArraySize (showText s)]

nameToType :: C.TypeSpecifier -> Either [SpecError] Type
nameToType = \case
  (C.TypeName (C.unCIdentifier -> n)) -> pure $ TypeName (T.pack n)
  C.Float -> pure Float
  C.Void -> pure Void
  C.Char Nothing -> pure Char
  C.Int C.Signed -> pure Int
  t -> Left [UnhandledCTypeSpecifier (showText t)]

stringToCType :: TypeParseContext -> Text -> Either [SpecError] (C.ParameterDeclaration C.CIdentifier)
stringToCType parseContext s =
  let s' = typeStringWorkarounds s
      r = C.runCParser parseContext
                       "typestring"
                       s'
                       (C.parseParameterDeclaration <* ReaderT (const Parsec.eof))
  in  case r of
        Left  parseError -> Left [TypeParseError s' (showText parseError)]
        Right d          -> Right d

showText :: Show a => a -> Text
showText = T.pack . show

specParserContext :: P.Spec -> Either [SpecError] TypeParseContext
specParserContext P.Spec {..} = validationToEither $ do
  let disallowedNames = ["int", "void", "char", "float"]
  let specTypeNames = filter (`notElem` disallowedNames)
        $ catMaybes (P.typeDeclTypeName <$> sTypes)
  specCTypeNames <- for specTypeNames $ \tn ->
    case C.cIdentifierFromString (T.unpack tn) of
      Left  e -> Failure [TypeNameParseError tn (T.pack e)]
      Right c -> pure c
  pure $ C.cCParserContext . HashSet.fromList $ specCTypeNames

-- | Drop the 'struct' keyword, it confuses our C type parser.
typeStringWorkarounds :: Text -> Text
typeStringWorkarounds =
      T.dropWhileEnd (== ';')
    . T.replace "VKAPI_PTR" ""
    . T.unwords
    . filter (/= "struct")
    . T.words

typeDepends :: Type -> [HaskellName]
typeDepends = \case
  Float                         -> []
  Void                          -> []
  Char                          -> []
  Int                           -> []
  Ptr t                         -> typeDepends t
  Array (NumericArraySize  _) t -> typeDepends t
  Array (SymbolicArraySize s) t -> Type s : typeDepends t
  TypeName t                    -> [Type t]
  Proto t ps -> typeDepends t ++ [ p | (_, pt) <- ps, p <- typeDepends pt ]
