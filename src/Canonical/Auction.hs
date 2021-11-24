{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Canonical.Auction
  ( script
  , Auction(..)
  , Datum(..)
  , Redeemer(..)
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger hiding (Datum, singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Ledger.Ada hiding (divide)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data Auction = Auction
  { auctionSeller :: !PubKeyHash
  , auctionDeadline :: !POSIXTime
  , auctionMinBid :: !Integer
  , auctionCurrency :: !CurrencySymbol
  , auctionToken :: !TokenName
  }

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

data Bid = Bid
  { bidBidder :: !PubKeyHash
  , bidAmount :: !Integer
  }

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data Action = PlaceBid Bid | Close

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

data Datum = Datum
  { datumAction :: !Auction
  , datumHighBid :: !(Maybe Bid)
  }

PlutusTx.unstableMakeIsData ''Datum
PlutusTx.makeLift ''Datum

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = getLovelace . fromValue

{-# INLINABLE ensureOnlyOneScriptInput #-}
ensureOnlyOneScriptInput :: ScriptContext -> Bool
ensureOnlyOneScriptInput ctx =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput i = case (txOutDatumHash . txInInfoResolved) i of
      Nothing -> False
      Just _ -> True
  in if length (filter isScriptInput $ txInfoInputs (scriptContextTxInfo ctx)) <= 1
       then True
       else False

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
{-
-}
{-# INLINABLE mkValidator #-}
mkValidator :: Datum -> Action -> ScriptContext -> Bool
mkValidator _datum action ctx =
  traceIfFalse "Only one script input allowed" (ensureOnlyOneScriptInput ctx)
    && case action of
      PlaceBid _bid -> False
      Close -> False

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
data Auctioning
instance Scripts.ValidatorTypes Auctioning where
  type instance DatumType Auctioning = Datum
  type instance RedeemerType Auctioning = Action

typedValidator :: Scripts.TypedValidator Auctioning
typedValidator =
  Scripts.mkTypedValidator @Auctioning
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

validator :: Validator
validator = Scripts.validatorScript typedValidator

-------------------------------------------------------------------------------
-- Entry point
-------------------------------------------------------------------------------
script :: PlutusScript PlutusScriptV1
script
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  $ validator
