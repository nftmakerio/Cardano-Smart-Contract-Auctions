# A NFT Auction Smart Contract

This repo contains the source for a Plutus NFT auction smart contract. The source for the smart contract is located in `src/Canonical/Auction.hs`.

The repo also contains an executable for compiling the smart contract in `app/Main.hs`.

## Building

The compile the code to a Plutus smart contract, run:

```bash
cabal run create-auction-sc
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
  , aPayoutPercentages :: !(A.Map PubKeyHash Integer)
  , aHighBid     :: !(Maybe Bid)
  }
```

A crucial note is the `aPayoutPercentages` field which is used to determine how to distribute the winning bid. Instead of going entirely to seller it is split up using the percentages in the map, as described in detail below.

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

Here is another example with percentages were 80%, 15% and 5% with 10,000,000 lovelaces:

```
seller: 7,578,948
marketplace: 1,421,052
royalty: 1,000,000
```

The extra 500,000 that was needed to give the royalty user 1 Ada, was taken equally from the seller and marketplace portions.

The exact calculation is as follows.

The percentages are sorted least to greatest. For each percentage the percentage is multiplied times the current total amount and divided by 1000 (remember the percentages are multiplied times a 10 so 2.5% is 25). If the portion is less than 1 Ada it is set to 1 Ada. The portion for this user is subtracted from the current total and their percentage is subtracted from the total percentages.

The loop starts again, with a new current total and a new current total percentages. The next percentage is adjusted by dividing by the new total percentage.

This process continues until the last element, which just get's whatever is left over.

Let's revisit our example above to see how it works.

The first time through the loop we multiple 50 * 10,000,000 and divide by 1,000 to get 500,000. This is less than 1 Ada (1,000,000) so we set the portion this user gets to 1 Ada. We subtract 1 Ada from the total to get 9 Ada and subtract 50 from the total percent (times 10) to get 950.

For the next iteration we multiple 150 * 9,000,000 and divide by 950 to get 1,421,052. This is greater than 1 Ada so we don't have to adjust it. We subtract 1,421,052 from 9,000,000 to get 7,578,947. We subtract 150 from 950 to get 800.

For the final iteration through the loop we just give the user the rest which is 7,578,947.

## Unit Tests

Because the divided up the price to various participants is so complicated there are unit tests to cover this logic specifically.

Run the unit tests by calling:

```bash
$ cabal test
```

## Full System Testing Prerequistes

Before testing you need to make sure you have `cardano-cli` installed and on your path, and it must be version 1.31.0 or greater. You will also need the json utility `jq` as well as `cardano-cli` helper `cardano-cli-balance-fixer` which can be downloaded here: https://github.com/Canonical-LLC/cardano-cli-balance-fixer

# Full System Tests

There are three large system tests that cover the main use cases and potential vulnerabilities we are aware of. The tests can be run on mainnet or testnet.

They take a long time to run, around 20 minutes, and can fail if the testnet is overloaded.

Luckily running them is easy:

```bash
$ ./scripts/tests/run-all-tests
```

The tests will start running. If the script errors one of the tests has failed. Otherwise all the tests have passed.
