set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/core/lock-tx.sh \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/start-expired-hash.txt) \
  "1758582 lovelace + 1 d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2.123456"
