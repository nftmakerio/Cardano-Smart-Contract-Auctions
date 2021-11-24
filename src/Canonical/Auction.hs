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
  , Action(..)
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import qualified Ledger as Ledger
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------
data Auction = Auction
  { auctionSeller :: !Ledger.PubKeyHash
  , auctionDeadline :: !Ledger.POSIXTime
  , auctionMinBid :: !Integer
  , auctionCurrency :: !Ledger.CurrencySymbol
  , auctionToken :: !Ledger.TokenName
  }

PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

instance Eq Auction where
  {-# INLINABLE (==) #-}
  x == y =
    (auctionSeller x == auctionSeller y)
      && (auctionDeadline x == auctionDeadline y)
      && (auctionMinBid x == auctionMinBid y)
      && (auctionCurrency x == auctionCurrency y)
      && (auctionToken x == auctionToken y)

data Bid = Bid
  { bidBidder :: !Ledger.PubKeyHash
  , bidAmount :: !Integer
  }

instance Eq Bid where
  {-# INLINABLE (==) #-}
  x == y =
    (bidBidder x == bidBidder y)
      && (bidAmount x == bidAmount y)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

data Action = PlaceBid Bid | Close

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

data Datum = Datum
  { datumAuction :: !Auction
  , datumHighBid :: !(Maybe Bid)
  }

PlutusTx.unstableMakeIsData ''Datum
PlutusTx.makeLift ''Datum

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
{-# INLINABLE lovelaces #-}
lovelaces :: Ledger.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
{-
-}
{-# INLINABLE mkValidator #-}
mkValidator :: Datum -> Action -> Ledger.ScriptContext -> Bool
mkValidator datum action ctx =
  traceIfFalse "wrong input value" correctInputValue
    && case action of
      PlaceBid b@Bid{..} ->
        traceIfFalse "bid too low" (sufficientBid bidAmount)
          && traceIfFalse "wrong output datum" (correctBidOutputDatum b)
          && traceIfFalse "wrong output value" (correctBidOutputValue bidAmount)
          && traceIfFalse "wrong refund" correctBidRefund
          && traceIfFalse "too late" correctBidSlotRange
      Close ->
        traceIfFalse "too early" correctCloseSlotRange
          && case datumHighBid datum of
            Nothing ->
              traceIfFalse "expected seller to get token" (getsValue seller tokenValue)
            Just Bid{..} ->
              traceIfFalse "expected highest bidder to get token" (getsValue bidBidder tokenValue)
                && traceIfFalse "expected seller to get highest bid" (getsValue seller . Ada.lovelaceValueOf $ bidAmount)

  where
    info :: Ledger.TxInfo
    info = Ledger.scriptContextTxInfo ctx

    input :: Ledger.TxInInfo
    input =
      case xs of
        [i] -> i
        _ -> traceError "expected exactly one script input"
      where
        xs = filter isScriptInput . Ledger.txInfoInputs $ info
        isScriptInput = maybe False (const True) . Ledger.txOutDatumHash . Ledger.txInInfoResolved

    inVal :: Ledger.Value
    inVal = Ledger.txOutValue . Ledger.txInInfoResolved $ input

    auction :: Auction
    auction = datumAuction datum

    seller :: Ledger.PubKeyHash
    seller = auctionSeller auction

    highBid :: Maybe Bid
    highBid = datumHighBid datum

    deadline :: Ledger.POSIXTime
    deadline = auctionDeadline auction

    tokenValue :: Ledger.Value
    tokenValue = Value.singleton (auctionCurrency auction) (auctionToken auction) 1

    correctInputValue :: Bool
    correctInputValue = inVal == case highBid of
      Nothing -> tokenValue
      Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf bidAmount

    sufficientBid :: Integer -> Bool
    sufficientBid amount = amount >= minBid
      where
        minBid = case highBid of
          Nothing -> auctionMinBid auction
          Just Bid{..} -> bidAmount + 1

    ownOutput   :: Ledger.TxOut
    outputDatum :: Datum
    (ownOutput, outputDatum) = case Ledger.getContinuingOutputs ctx of
      [o] -> case Ledger.txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h -> case Ledger.findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Ledger.Datum d) ->  case PlutusTx.fromBuiltinData d of
            Just ad' -> (o, ad')
            Nothing  -> traceError "error decoding data"
      _ -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b =
      (datumAuction outputDatum == auction)
        && (datumHighBid outputDatum == Just b)

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
      Ledger.txOutValue ownOutput == tokenValue <> Ada.lovelaceValueOf amount

    correctBidRefund :: Bool
    correctBidRefund = case highBid of
      Nothing -> True
      Just Bid{..} ->
        case os of
          [o] -> Ledger.txOutValue o == Ada.lovelaceValueOf bidAmount
          _   -> traceError "expected exactly one refund output"
        where
          os = filter ((Ledger.pubKeyHashAddress bidBidder ==) . Ledger.txOutAddress) . Ledger.txInfoOutputs $ info

    correctBidSlotRange :: Bool
    correctBidSlotRange = Ledger.to deadline `Ledger.contains` Ledger.txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = Ledger.from deadline `Ledger.contains` Ledger.txInfoValidRange info

    getsValue :: Ledger.PubKeyHash -> Ledger.Value -> Bool
    getsValue h v = any same . Ledger.txInfoOutputs $ info
      where
        same Ledger.TxOut{..} =
          (Ledger.pubKeyHashAddress h == txOutAddress)
            && (v == txOutValue)

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

validator :: Ledger.Validator
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
