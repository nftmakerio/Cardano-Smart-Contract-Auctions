set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

DATUM_PREFIX=${DATUM_PREFIX:-0}

$baseDir/core/bid-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/buyer.addr) \
  ~/$BLOCKCHAIN_PREFIX/buyer.skey \
  d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456 \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/start.json \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/start-hash.txt) \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/bid-1-hash.txt) \
  $baseDir/$BLOCKCHAIN_PREFIX/datums/$DATUM_PREFIX/bid-1.json \
  7000000 \
  $baseDir/redeemers/bid-1.json