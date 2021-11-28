# A NFT Auction Smart Contract

This repo contains the source for a Plutus NFT auction smart contract. The source for the smart contract is located in `src/Canonical/Auction.hs`.

The repo also contains an executable for compiling the smart contract in `app/Main.hs`.

## Building

The compile the code to a Plutus smart contract, run:

```bash
cabal run
```

This will write a file to `scripts/auction.plutus`

A `shell.nix` is also providing for nix users.

## Creating the Script Address

After compiling the smart contract, it is necessary to make a script address.

First source either the testnet or mainnet environment variables.

For testnet

```
$ source scripts/envars/testnet-env.envvars
```

For mainnet

```
$ source scripts/envars/mainnet-env.envvars
```

The environment variable files set `CARDANO_NODE_SOCKET_PATH` to the path of the appropriate Daedalus socket file (either Testnet Daedalus or the regular mainnet Daedalus). It you run a `cardano-node` on your own you should set this environment variable to your socket file location after sourcing the environment variable file.

Next, run:

```bash
scripts/hash-plutus.sh
```

## Example Transactions

Example transactions can be found in `scripts/core`. The scripts are used by other scripts in `scripts/happy-path` which demonstrates how to start, bid and close the auction.

## Example Redeemers and Datums

Example redeemers are found in `scripts/redeemers` and example datums are found in `scripts/datums`.

Here is the Haskell type of the Datum

```haskell
data Auction = Auction
  { aSeller      :: !PubKeyHash
  , aDeadline    :: !POSIXTime
  , aMinBid      :: !Integer
  , aCurrency    :: !CurrencySymbol
  , aToken       :: !TokenName
  , aPercentages :: !(A.Map PubKeyHash Integer)
  , aHighBid     :: !(Maybe Bid)
  }
```

A crucial note is the `aPercentages` field which is used to determine how to distribute the winning bid. Instead of going entirely to seller it is split up using the percentages in the map, as described in detail below.

## Understanding the Price Calculation

When the auction is finished, assuming there is winning bid, the funds are distributed to multiple parties as described by the percentages map.

The map is collection of pairs of public key hash and percentage. The percentages are stored as integers times 1000, so 2.5% would be stored as 25.

The calculation to determine how much to give each party is not as straightforward as one would imagine because of minimum Ada UTxO requirements.

Regardless of the percentage, every participant recieves a minimum of 1 Ada.

For example, if there were three (seller, marketplace and royalty) users and percentages were 95%, 2.5% and 2.5%, 10,000,000 lovelaces would distributed as follows:

```
seller: 8,000,000
marketplace: 1,000,000
royalty: 1,000,000
```

This is in contrast to the split as dicated by the percentages directly, which would have been:

```
seller: 9,500,000
marketplace: 250,000
royalty: 250,000
```

## Testing Prerequistes

Before testing you need to make sure you have `cardano-cli` installed and on your path, and it must be version 1.31.0 or greater.
