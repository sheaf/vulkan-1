{-# language TemplateHaskellQuotes #-}
module Render.FuncPointer
  where

import           Relude                  hiding ( Reader
                                                , ask
                                                , lift
                                                )
import           Data.Text.Prettyprint.Doc
import           Language.Haskell.TH.Syntax
import           Polysemy
import           Polysemy.Reader
import           Foreign.Ptr

import           Spec.Parse
import           Haskell                       as H
import           Error
import           Render.Element
import           Render.Type
import           Render.SpecInfo
import           CType

renderFuncPointer
  :: (HasErr r, Member (Reader RenderParams) r, HasSpecInfo r)
  => FuncPointer
  -> Sem r RenderElement
renderFuncPointer FuncPointer {..} = contextShow (unCName fpName) $ do
  RenderParams {..} <- ask
  fmap identicalBoot . genRe ("func pointer " <> unCName fpName) $ do
    let p = mkTyName fpName
        n = mkFuncPointerName fpName
    tDoc    <- renderType =<< cToHsType DoPreserve =<< stripPtr fpType
    tPtrDoc <- renderType (ConT ''FunPtr :@ ConT (typeName n))
    tellExport (EType p)
    tellExport (EType n)
    tellDoc
      $   "type"
      <+> pretty n
      <+> "="
      <+> tDoc
      <>  line
      <>  "type"
      <+> pretty p
      <+> "="
      <+> tPtrDoc

stripPtr :: HasErr r => CType -> Sem r CType
stripPtr = \case
  Ptr _ t -> pure t
  _ -> throw "trying to strip the pointer from a non-pointer type"

