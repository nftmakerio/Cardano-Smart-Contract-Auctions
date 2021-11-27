set -eux

thisDir=$(dirname "$0")
baseDir=$thisDir/..

sellerAddr=$1
signingKey=$2
value=$3
datumFile=$4
datumHash=$5
winningBuyer=$6
sellerAmount=$7
sellerAddr=$8
royaltyAmount=$9
royaltyAddr="${10}"
marketplaceAmount="${11}"
marketplaceAddr="${12}"


nftValidatorFile=$baseDir/auction.plutus
scriptHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/auction.addr)

$baseDir/hash-plutus.sh
bodyFile=temp/close-tx-body.01
outFile=temp/close-tx.01
redeemerFile="$baseDir/redeemers/close.json"
utxoScript=$(scripts/query/sc.sh | grep $datumHash | grep $value | head -n 1 | cardano-cli-balance-fixer parse-as-utxo)
output1="1724100 lovelace + 1 $value"
currentSlot=$(cardano-cli query tip $BLOCKCHAIN | jq .slot)
startSlot=$(($currentSlot-1))
nextTenSlots=$(($currentSlot+1000))

cardano-cli transaction build \
    --alonzo-era \
    $BLOCKCHAIN \
    $(cardano-cli-balance-fixer input --address $sellerAddr $BLOCKCHAIN ) \
    --tx-in $utxoScript \
    --tx-in-script-file $nftValidatorFile \
    --tx-in-datum-file $datumFile \
    --tx-in-redeemer-file $redeemerFile \
    --required-signer $signingKey \
    --tx-in-collateral $(cardano-cli-balance-fixer collateral --address $sellerAddr $BLOCKCHAIN) \
    --tx-out "$winningBuyer + $output1" \
    --tx-out "$sellerAddr + $sellerAmount lovelace + $(cardano-cli-balance-fixer change --address $sellerAddr $BLOCKCHAIN)" \
    --tx-out "$royaltyAddr + $royaltyAmount lovelace "  \
    --tx-out "$marketplaceAddr + $marketplaceAmount lovelace "  \
    --change-address $sellerAddr \
    --protocol-params-file scripts/$BLOCKCHAIN_PREFIX/protocol-parameters.json \
    --invalid-before $startSlot\
    --invalid-hereafter $nextTenSlots \
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