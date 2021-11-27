set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

while ! $baseDir/query/seller.sh | grep d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2
do
  echo waiting for asset
  sleep 2
done
