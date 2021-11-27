set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

while true
do
    res=$($baseDir/query/sc.sh | grep d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2)
    if [ "$res" = "" ]
    then
      echo waiting for asset
      sleep 2
    else
      exit
    fi
done
