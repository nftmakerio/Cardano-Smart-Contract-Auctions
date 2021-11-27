set -eux
thisDir=$(dirname "$0")

cardano-cli transaction hash-script-data \
  --script-data-file $thisDir/$BLOCKCHAIN_PREFIX/datums/start.json \
  > $thisDir/$BLOCKCHAIN_PREFIX/datums/start-hash.txt

cardano-cli transaction hash-script-data \
  --script-data-file $thisDir/$BLOCKCHAIN_PREFIX/datums/start-expired.json \
  > $thisDir/$BLOCKCHAIN_PREFIX/datums/start-expired-hash.txt

cardano-cli transaction hash-script-data \
  --script-data-file $thisDir/$BLOCKCHAIN_PREFIX/datums/bid-1.json \
  > $thisDir/$BLOCKCHAIN_PREFIX/datums/bid-1-hash.txt

cardano-cli transaction hash-script-data \
  --script-data-file $thisDir/$BLOCKCHAIN_PREFIX/datums/bid-2.json \
  > $thisDir/$BLOCKCHAIN_PREFIX/datums/bid-2-hash.txt
