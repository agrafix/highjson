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

makeDeclareNamedSchema ::
    AllHave ToSchema ts => T.Text -> JsonSpec k ts -> Proxy k -> DeclM NamedSchema
makeDeclareNamedSchema sn spec _ =
    do (props, reqs) <- computeProperties (j_fields spec)
       pure $ NamedSchema (Just sn) $
           mempty
           & type_ .~ SwaggerObject
           & properties .~ props
           & required .~ reqs

computeProperties ::
    forall k ts. AllHave ToSchema ts
    => FieldSpec k ts
    -> DeclM (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
computeProperties fs =
    go fs (mempty, mempty)
    where
      go ::
          forall qs. AllHave ToSchema qs
          => FieldSpec k qs
          -> (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
          -> DeclM (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
      go spec (props, reqs) =
          case spec of
            EmptySpec ->
                pure (props, reqs)
            (key :: FieldKey k t) :+: rest ->
                do fieldSchema <- declareSchemaRef (Proxy :: Proxy t)
                   let fld =
                           IOM.singleton (k_key (fk_sk key)) fieldSchema
                       reqs' =
                           if k_req (fk_sk key)
                           then (k_key (fk_sk key) : reqs)
                           else reqs
                   go rest (fld <> props, reqs')
