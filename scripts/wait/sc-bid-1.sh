set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

bid1Hash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/bid-1-hash.txt)

while ! $baseDir/query/sc.sh | grep bid1Hash
do
  echo waiting for asset
  sleep 2
done
