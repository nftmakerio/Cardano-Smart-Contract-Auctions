set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

bidHash=$(cat $baseDir/$BLOCKCHAIN_PREFIX/datums/bid-2-hash.txt)

while ! $($baseDir/query/sc.sh | grep bidHash)
do
  echo waiting for asset
  sleep 2
done
