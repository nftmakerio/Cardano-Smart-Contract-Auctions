set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

bodyFile=temp/buy-tx-body.01
outFile=temp/buy-tx.01
nftValidatorFile="$baseDir/market.plutus"
value0='d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456'
value1='79f2c52b856e89098130f14e96f3768257dbfa71756586b60424ca1d.123456'
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)
royaltyAddr=$(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr)
signingKey=~/$BLOCKCHAIN_PREFIX/buyer.skey
sellerAmount=8000000
marketPlaceAmount=1000000
royaltyAmount=1000000
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/market.addr)
datumFile0="$baseDir/$BLOCKCHAIN_PREFIX/buy-cancel-datum.json"
datumHash0=$(cat $baseDir/$BLOCKCHAIN_PREFIX/buy-cancel-datum-hash.txt)
datumFile1="$baseDir/$BLOCKCHAIN_PREFIX/buy-cancel-datum-1.json"
datumHash1=$(cat $baseDir/$BLOCKCHAIN_PREFIX/buy-cancel-datum-1-hash.txt)

redeemerFile="$baseDir/redeemers/buy-redeemer.json"

utxoScript0=$(scripts/query/sc.sh | grep $datumHash0 | grep $value0 | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
utxoScript1=$(scripts/query/sc.sh | grep $datumHash1 | grep $value1 | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)

buyerPaidValue0="$sellerAmount lovelace"
buyerPaidValue1="$marketPlaceAmount lovelace"
buyerPaidValue2="$royaltyAmount lovelace"

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
