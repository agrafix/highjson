{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Data.HighJson.Swagger
    ( makeDeclareNamedSchema
    )
where

import Control.Lens
import Data.HVect (AllHave)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HighJson
import Data.HighJson.Serialiser
import Data.Monoid
import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import qualified Data.HashMap.Strict.InsOrd as IOM
import qualified Data.Text as T

type DeclM = Declare (Definitions Schema)

makeDeclareNamedSchema :: AllHave ToSchema ts => T.Text -> JsonSpec k ts -> Proxy k -> DeclM NamedSchema
makeDeclareNamedSchema sn spec _ =
    do props <- computeProperties (j_fields spec)
       pure $ NamedSchema (Just sn) $
           mempty
           & type_ .~ SwaggerObject
           & properties .~ props

computeProperties :: forall k ts. AllHave ToSchema ts => FieldSpec k ts -> DeclM (InsOrdHashMap T.Text (Referenced Schema))
computeProperties fs =
    go fs mempty
    where
      go ::
          forall qs. AllHave ToSchema qs
          => FieldSpec k qs
          -> InsOrdHashMap T.Text (Referenced Schema)
          -> DeclM (InsOrdHashMap T.Text (Referenced Schema))
      go spec accum =
          case spec of
            EmptySpec ->
                pure accum
            (key :: FieldKey k t) :+: rest ->
                do fieldSchema <- declareSchemaRef (Proxy :: Proxy t)
                   let fld =
                           IOM.singleton (k_key (fk_sk key)) fieldSchema
                   go rest (fld <> accum)
