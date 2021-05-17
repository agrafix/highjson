{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Data.HighJson.Swagger
    ( makeDeclareNamedSchema, makeDeclareNamedSchema', DeclM
    , IsValidSwaggerType, AllAre, NoneAre
    )
where

import Control.Lens
import Data.HVect (AllHave)
import Data.HashMap.Strict.InsOrd (InsOrdHashMap)
import Data.HighJson
import Data.Kind
import Data.Proxy
import Data.Swagger
import Data.Swagger.Declare
import qualified Data.HashMap.Strict.InsOrd as IOM
import qualified Data.Text as T

type DeclM = Declare (Definitions Schema)

type family AllAre x (xs :: [*]) :: Bool where
    AllAre x (x ': xs) = AllAre x xs
    AllAre x '[] = 'True

type family NoneAre x (xs :: [*]) :: Bool where
    NoneAre x (x ': xs) = 'False
    NoneAre x (y ': xs) = NoneAre x xs
    NoneAre x '[] = 'True

-- | Not all valid Haskell types have a valid swagger mapping. Simple records
-- are fine, but sum types should be either "real" Enums or every option must
-- contain a value. For more information see the swagger2 haskell package.
type family IsValidSwaggerType ty (ts :: [*]) :: Constraint where
    IsValidSwaggerType 'SpecRecord xs = 'True ~ 'True
    IsValidSwaggerType 'SpecSum xs = NoneAre () xs ~ 'True
    IsValidSwaggerType 'SpecEnum xs = AllAre () xs ~ 'True

-- | Automatically generate a 'NamedSchema' from a 'HighSpec'
makeDeclareNamedSchema ::
    (AllHave ToSchema ts, AllHave ToJSON ts, IsValidSwaggerType ty ts)
    => HighSpec k ty ts
    -> f k
    -> DeclM NamedSchema
makeDeclareNamedSchema spec = makeDeclareNamedSchema' spec Nothing

-- | Automatically generate a 'NamedSchema' from a 'HighSpec' while optionally
-- providing an example value
makeDeclareNamedSchema' ::
    (AllHave ToSchema ts, AllHave ToJSON ts, IsValidSwaggerType ty ts)
    => HighSpec k ty ts
    -> Maybe k
    -- ^ example value
    -> f k
    -> DeclM NamedSchema
makeDeclareNamedSchema' spec exVal _ =
    case hs_bodySpec spec of
      BodySpecRecord r ->
          do (props, reqs) <- computeRecProperties r
             pure $ NamedSchema (Just $ hs_name spec) $
                 mempty
                 & type_ ?~ SwaggerObject
                 & description .~ hs_description spec
                 & properties .~ props
                 & required .~ reqs
                 & maxProperties .~ Just (fromIntegral $ length props)
                 & minProperties .~ Just (fromIntegral $ length reqs)
                 & example .~ fmap (jsonSerializer spec) exVal
      BodySpecSum r ->
          do (props, reqs) <- computeSumProperties r
             pure $ NamedSchema (Just $ hs_name spec) $
                 mempty
                 & type_ ?~ SwaggerObject
                 & description .~ hs_description spec
                 & properties .~ props
                 & required .~ reqs
                 & maxProperties .~ Just 1
                 & minProperties .~ Just 1
                 & example .~ fmap (jsonSerializer spec) exVal
      BodySpecEnum r ->
          let ps =
                  mempty
                  & type_ ?~ SwaggerString
                  & enum_ .~ Just (map (toJSON . eo_jsonKey) (es_options r))
          in pure $ NamedSchema (Just $ hs_name spec) $
             mempty
             & type_ ?~ SwaggerString
             & description .~ hs_description spec
             & example .~ fmap (jsonSerializer spec) exVal
             & paramSchema .~ ps

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
                           then rf_jsonKey key : reqs
                           else reqs
                   go rest (fld <> props, reqs')
