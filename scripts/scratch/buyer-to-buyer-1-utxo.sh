set -eux

bodyFile=temp/consolidate-tx-body.01
signingKey=~/$BLOCKCHAIN_PREFIX/benefactor.skey
outFile=temp/consolidate-tx.01
senderAddr=$(cat ~/$BLOCKCHAIN_PREFIX/benefactor.addr)
sellerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/seller.addr)
buyerAddr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr)
buyer1Addr=$(cat ~/$BLOCKCHAIN_PREFIX/buyer1.addr)
marketplaceAddr=$(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)

cardano-cli transaction build \
  --alonzo-era \
  $BLOCKCHAIN \
  --tx-in 4546d3e59e5bac90dd4b0b732b8a2bc2a96eded09fe3aff409847aaa93b5b301#0 \
  --tx-out "$sellerAddr +50000000 lovelace" \
  --tx-out "$buyerAddr +50000000 lovelace" \
  --tx-out "$buyer1Addr +50000000 lovelace" \
  --tx-out "$marketplaceAddr +50000000 lovelace" \
  --change-address $senderAddr \
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
