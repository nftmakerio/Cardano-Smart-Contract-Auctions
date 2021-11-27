set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../


$baseDir/core/close-successfully-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456" \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/bid-1.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/bid-1-hash.txt) \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  8000000 \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  1000000 \
  $(cat ~/$BLOCKCHAIN_PREFIX/royalities.addr) \
  1000000 \
  $(cat ~/$BLOCKCHAIN_PREFIX/marketplace.addr)
