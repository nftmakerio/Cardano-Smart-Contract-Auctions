set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../


$baseDir/core/close-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456" \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/start.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/start-hash.txt)
