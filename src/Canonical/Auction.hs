{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}



module Canonical.Auction
  ( script
  , Auction(..)
  , Action(..)
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import           Ledger
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Ledger.Ada as Ada
import qualified Ledger.Value as Value
import qualified PlutusTx.AssocMap as A
import Plutus.V1.Ledger.Credential


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Bid = Bid
  { bidBidder :: !PubKeyHash
  , bidAmount :: !Integer
  }

data Auction = Auction
  { aSeller      :: !PubKeyHash
  , aDeadline    :: !POSIXTime
  , aMinBid      :: !Integer
  , aCurrency    :: !CurrencySymbol
  , aToken       :: !TokenName
  , aPercentages :: !(A.Map PubKeyHash Integer)
  , aHighBid     :: !(Maybe Bid)
  }

data Action = PlaceBid !Bid | Close
-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
PlutusTx.unstableMakeIsData ''Auction
PlutusTx.makeLift ''Auction

instance Eq Auction where
  {-# INLINABLE (==) #-}
  x == y
    =  (aSeller      x == aSeller      y)
    && (aDeadline    x == aDeadline    y)
    && (aMinBid      x == aMinBid      y)
    && (aCurrency    x == aCurrency    y)
    && (aToken       x == aToken       y)
    && (aPercentages x == aPercentages y)
    && (aHighBid     x == aHighBid     y)

instance Eq Bid where
  {-# INLINABLE (==) #-}
  x == y
    =  (bidBidder x == bidBidder y)
    && (bidAmount x == bidAmount y)

PlutusTx.unstableMakeIsData ''Bid
PlutusTx.makeLift ''Bid

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

-------------------------------------------------------------------------------
-- Base Extras
-------------------------------------------------------------------------------
{-# INLINABLE drop #-}
drop :: Integer -> [a] -> [a]
drop n l@(_:xs) =
    if n <= 0 then l
    else drop (n-1) xs
drop _ [] = []

{-# INLINABLE merge #-}
merge :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
merge as@(a:as') bs@(b:bs') =
    if snd a <= snd b
    then a:(merge as'  bs)
    else b:(merge as bs')
merge [] bs = bs
merge as [] = as

{-# INLINABLE mergeSort #-}
mergeSort :: [(PubKeyHash, Integer)] -> [(PubKeyHash, Integer)]
mergeSort xs =
    let n = length xs
    in if n > 1
       then let n2 = n `divide` 2
            in merge (mergeSort (take n2 xs)) (mergeSort (drop n2 xs))
       else xs

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------
{-# INLINABLE lovelaces #-}
type Percent = Integer
type Lovelaces = Integer

lovelaces :: Value -> Lovelaces
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE sortPercents #-}
sortPercents :: A.Map PubKeyHash Percent -> [(PubKeyHash, Percent)]
sortPercents = mergeSort . A.toList

{-# INLINABLE minAda #-}
minAda :: Lovelaces
minAda = 1_000_000
-- This needs to track the diff between the value and the min and subtract it from
-- the "pot". which is what is multiplied by the percent.
{-# INLINABLE calculateAll #-}
calculateAll :: Integer -> [(PubKeyHash, Percent)] -> [(PubKeyHash, Lovelaces)]
calculateAll total percents = go total total percents where
  go left pot = \case
    [] -> traceError "No seller percentage specified"
    [(pkh, _)] -> [(pkh, left)]
    (pkh, percent) : rest ->
      let percentOfPot = percentOwed pot percent
          percentOf = max minAda percentOfPot
      in (pkh, percentOf) : go (left - percentOf) (pot - max 0 (minAda - percentOfPot)) rest

{-# INLINABLE percentOwed #-}
percentOwed :: Lovelaces -> Percent -> Lovelaces
percentOwed inVal pct = (inVal * pct) `divide` 1000

-- Iterate throught the Config Map and check that each
-- address gets the correct percentage
{-# INLINABLE outputValid #-}
outputValid :: Lovelaces -> TxInfo -> A.Map PubKeyHash Percent -> Bool
outputValid total info = all (paidPercentOwed info) . calculateAll total . sortPercents

-- For a given address and percentage pair, verify
-- they received greater or equal to their percentage
-- of the input.
{-# INLINABLE paidPercentOwed #-}
paidPercentOwed :: TxInfo -> (PubKeyHash, Lovelaces) -> Bool
paidPercentOwed info (addr, owed) =
  lovelaces (valuePaidTo info addr) >= owed

{-# INLINABLE isScriptAddress #-}
isScriptAddress :: Address -> Bool
isScriptAddress Address { addressCredential } = case addressCredential of
  ScriptCredential _ -> True
  _ -> False

{-# INLINABLE getOnlyScriptInput #-}
getOnlyScriptInput :: TxInfo -> Value
getOnlyScriptInput info =
  let
    isScriptInput :: TxInInfo -> Bool
    isScriptInput = isScriptAddress . txOutAddress . txInInfoResolved

    input = case filter isScriptInput . txInfoInputs $ info of
      [i] -> i
      _ -> traceError "expected exactly one script input"

  in txOutValue . txInInfoResolved $ input
-------------------------------------------------------------------------------
-- Validator
-------------------------------------------------------------------------------
{-
-}
{-# INLINABLE mkValidator #-}
mkValidator :: Auction -> Action -> ScriptContext -> Bool
mkValidator auction@Auction {..} action ctx =
  traceIfFalse "wrong input value" correctInputValue
    && case action of
      PlaceBid b@Bid{..}
          -> traceIfFalse "bid too low"        (sufficientBid bidAmount)
          && traceIfFalse "wrong output datum" (correctBidOutputDatum b)
          && traceIfFalse "wrong output value" (correctBidOutputValue bidAmount)
          && traceIfFalse "wrong refund"       correctBidRefund
          && traceIfFalse "too late"           correctBidSlotRange
      Close ->
        traceIfFalse "too early" correctCloseSlotRange
          && case aHighBid of
            Nothing ->
              traceIfFalse "expected seller to get token" (getsValue aSeller tokenValue)
            Just Bid{..}
              -> traceIfFalse "expected highest bidder to get token" (getsValue bidBidder tokenValue)
              && traceIfFalse "expected seller to get highest bid" (outputValid bidAmount info aPercentages)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    inVal :: Value
    inVal = getOnlyScriptInput info

    tokenValue :: Value
    tokenValue = Value.singleton aCurrency aToken 1

    correctInputValue :: Bool
    correctInputValue = inVal `Value.geq` case aHighBid of
      Nothing -> tokenValue
      Just Bid{..} -> tokenValue <> Ada.lovelaceValueOf bidAmount

    sufficientBid :: Integer -> Bool
    sufficientBid amount =
      let
        minBid = case aHighBid of
          Nothing -> aMinBid
          Just Bid{..} -> bidAmount + 1

      in amount >= minBid

    ownOutput   :: TxOut
    outputDatum :: Auction

    (ownOutput, outputDatum) = case getContinuingOutputs ctx of
      [o] -> case txOutDatumHash o of
        Nothing -> traceError "wrong output type"
        Just h -> case findDatum h info of
          Nothing -> traceError "datum not found"
          Just (Datum d) ->  case PlutusTx.fromBuiltinData d of
            Just ad' -> (o, ad')
            Nothing  -> traceError "error decoding data"
      _ -> traceError "expected exactly one continuing output"

    correctBidOutputDatum :: Bid -> Bool
    correctBidOutputDatum b
      = outputDatum == (auction { aHighBid = Just b })

    correctBidOutputValue :: Integer -> Bool
    correctBidOutputValue amount =
      txOutValue ownOutput `Value.geq` (tokenValue <> Ada.lovelaceValueOf amount)

    correctBidRefund :: Bool
    correctBidRefund = case aHighBid of
      Nothing -> True
      Just Bid{..} ->
        case os of
          [o] -> lovelaces (txOutValue o) >= bidAmount
          _   -> traceError "expected exactly one refund output"
        where
          os = filter
            ( (pubKeyHashAddress bidBidder ==)
            . txOutAddress
            )
            . txInfoOutputs $ info

    correctBidSlotRange :: Bool
    correctBidSlotRange = aDeadline `after` txInfoValidRange info

    correctCloseSlotRange :: Bool
    correctCloseSlotRange = aDeadline `before` txInfoValidRange info

    getsValue :: PubKeyHash -> Value -> Bool
    getsValue h v = any same . txInfoOutputs $ info
      where
        same TxOut{..} =
          (pubKeyHashAddress h == txOutAddress)
            && (txOutValue `Value.geq` v)

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------
data Auctioning
instance Scripts.ValidatorTypes Auctioning where
  type instance DatumType Auctioning = Auction
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
