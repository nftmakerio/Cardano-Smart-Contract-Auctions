set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

bodyFile=temp/buy-tx-body.01
outFile=temp/buy-tx.01
nftValidatorFile="$baseDir/auction.plutus"
value0='d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456'
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
signingKey=~/$BLOCKCHAIN_PREFIX/buyer.skey
bidAmount=10000000
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)
datumFile0="$baseDir/$BLOCKCHAIN_PREFIX/start.json"
datumHash0=$(cat $baseDir/$BLOCKCHAIN_PREFIX/start-hash.txt)
datumFile1=$datumFile0

datumHash0=$datumHash1
value1=$value0

redeemerFile="$baseDir/redeemers/bid-1.json"

utxoScript0=$(scripts/query/sc.sh | grep $datumHash0 | grep $value0 | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
utxoScript1=$(scripts/query/sc.sh | grep $datumHash1 | grep $value1 | tail -n 1 | cardano-cli-balance-fixer parse-as-utxo)


cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $buyerAddr $BLOCKCHAIN ) \
    --tx-in $utxoScript0 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile0 \
    --tx-in-redeemer-file $redeemerFile \
    --tx-in $utxoScript1 \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile1 \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $buyerAddr $BLOCKCHAIN) \
    --tx-out "$sellerAddr + $buyerPaidValue0" \
    --tx-out "$buyerAddr + 1758582 lovelace + 1 $value0" \
    --tx-out "$buyerAddr + 1758582 lovelace + 1 $value1" \
    --tx-out "$marketplaceAddr + $buyerPaidValue1" \
    --tx-out "$royaltyAddr + $buyerPaidValue2" \
    --tx-out "$buyerAddr + 1758582 lovelace + $(cardano-cli-balance-fixer change --address $buyerAddr $BLOCKCHAIN -o"$buyerPaidValue0" -o"$buyerPaidValue1" -o"$buyerPaidValue2")" \
    --change-address $buyerAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --out-file $bodyFile

echo "saved transaction to $bodyFile"

cardano-cli transaction sign \
   --tx-body-file $bodyFile \
   --signing-key-file $signingKey \
   $BLOCKCHAIN \
   --out-file $outFile

echo "signed transaction and saved as $outFile"

cardano-cli transaction submit \
  $BLOCKCHAIN \
  --tx-file $outFile

echo "submitted transaction"

echo
