set -eu
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$baseDir/core/sell-tx.sh \
  $baseDir/market.plutus \
  $(cat ~/$BLOCKCHAIN_PREFIX/seller.addr) \
  ~/$BLOCKCHAIN_PREFIX/seller.skey \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/buy-cancel-datum-1-hash.txt) \
  "1758582 lovelace + 1 79f2c52b856e89098130f14e96f3768257dbfa71756586b60424ca1d.123456" \
  $(cat $baseDir/$BLOCKCHAIN_PREFIX/market.addr)
