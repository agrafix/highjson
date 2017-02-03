{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Data.HighJson.Swagger
    ( makeDeclareNamedSchema, makeDeclareNamedSchema', DeclM
    )
where

import Control.Lens
import Data.HVect (AllHave)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HighJson
import Data.Monoid
import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import qualified Data.HashMap.Strict.InsOrd as IOM
import qualified Data.Text as T

type DeclM = Declare (Definitions Schema)

-- | Automatically generate a 'NamedSchema' from a 'HighSpec'
makeDeclareNamedSchema ::
    (AllHave ToSchema ts, AllHave ToJSON ts)
    => HighSpec k ts
    -> Proxy k
    -> DeclM NamedSchema
makeDeclareNamedSchema spec = makeDeclareNamedSchema' spec Nothing

-- | Automatically generate a 'NamedSchema' from a 'HighSpec' while optionally
-- providing an example value
makeDeclareNamedSchema' ::
    (AllHave ToSchema ts, AllHave ToJSON ts)
    => HighSpec k ts
    -> Maybe k
    -- ^ example value
    -> Proxy k
    -> DeclM NamedSchema
makeDeclareNamedSchema' spec exVal _ =
    do (props, reqs) <-
           case hs_bodySpec spec of
             BodySpecRecord r -> computeRecProperties r
             BodySpecSum r -> computeSumProperties r
       let (minProps, maxProps) =
               case hs_bodySpec spec of
                 BodySpecSum _ -> (Just 1, Just 1)
                 BodySpecRecord _ ->
                     (Just (fromIntegral $ length reqs), Just (fromIntegral $ length props))
       pure $ NamedSchema (Just $ hs_name spec) $
           mempty
           & type_ .~ SwaggerObject
           & description .~ hs_description spec
           & properties .~ props
           & required .~ reqs
           & maxProperties .~ maxProps
           & minProperties .~ minProps
           & example .~ fmap (jsonSerializer spec) exVal

computeSumProperties ::
    forall k ts. AllHave ToSchema ts
    => SumSpec k ts
    -> DeclM (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
computeSumProperties fs =
    go (ss_options fs) (mempty, mempty)
    where
      go ::
          forall qs. AllHave ToSchema qs
          => SumOptions k qs
          -> (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
          -> DeclM (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
      go spec (props, reqs) =
          case spec of
            SOEmpty ->
                pure (props, reqs)
            (key :: SumOption k t) :|: rest ->
                do fieldSchema <- declareSchemaRef (Proxy :: Proxy t)
                   let fld =
                           IOM.singleton (so_jsonKey key) fieldSchema
                   go rest (fld <> props, reqs)

computeRecProperties ::
    forall k ts. AllHave ToSchema ts
    => RecordSpec k ts
    -> DeclM (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
computeRecProperties fs =
    go (rs_fields fs) (mempty, mempty)
    where
      go ::
          forall qs. AllHave ToSchema qs
          => RecordFields k qs
          -> (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
          -> DeclM (InsOrdHashMap T.Text (Referenced Schema), [ParamName])
      go spec (props, reqs) =
          case spec of
            RFEmpty ->
                pure (props, reqs)
            (key :: RecordField k t) :+: rest ->
                do fieldSchema <- declareSchemaRef (Proxy :: Proxy t)
                   let fld =
                           IOM.singleton (rf_jsonKey key) fieldSchema
                       reqs' =
                           if not (rf_optional key)
                           then (rf_jsonKey key : reqs)
                           else reqs
                   go rest (fld <> props, reqs')
