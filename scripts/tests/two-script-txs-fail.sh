set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
startTime0=$(date +%s)

$baseDir/wait/until-next-block.sh

echo Mint 1
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

echo Start Auction 1
$baseDir/happy-path/lock-tx.sh 500000
sleep 2
$baseDir/wait/until-next-block.sh

echo First Bid 1
$baseDir/happy-path/bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

echo Mint 2
$baseDir/minting/mint-0-policy.sh
sleep 2
$baseDir/wait/until-next-block.sh

startTime1=$(date +%s)
export DATUM_PREFIX=1

echo Start Auction 2
$baseDir/happy-path/lock-tx.sh 550000 1
sleep 2
$baseDir/wait/until-next-block.sh

echo First Bid 2
$baseDir/happy-path/seller-bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

export DATUM_PREFIX=0

echo Second Bid 1
$baseDir/happy-path/bid-2-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime0))
sleepTime=$((605 - $elapsedTime))
sleep $sleepTime

"$baseDir/failure-cases/outbid-close-tx.sh" || {
    detected=true
}

if [ detected == false ]; then
  exit 1
fi

export DATUM_PREFIX=1


echo Success!
