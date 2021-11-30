set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
startTime0=$(date +%s)

$baseDir/wait/until-next-block.sh

echo Mint 1
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

echo Start Auction 1
$baseDir/happy-path/lock-tx.sh 600000
$baseDir/wait/until-next-block.sh

echo First Bid 1
$baseDir/happy-path/bid-1-tx.sh
$baseDir/wait/until-next-block.sh

echo Mint 2
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

startTime1=$(date +%s)
export DATUM_PREFIX=1

echo Start Auction 2
$baseDir/happy-path/lock-tx.sh 700000 1
$baseDir/wait/until-next-block.sh

echo First Bid 2
$baseDir/happy-path/bid-1-tx.sh
$baseDir/wait/until-next-block.sh

echo Failed Double Outbid
detected=false

"$baseDir/failure-cases/double-outbid-tx.sh" || {
    detected=true
}

if [ detected == false ]; then
  exit 1
fi

export DATUM_PREFIX=0

echo Second Bid 1
$baseDir/happy-path/bid-2-tx.sh
$baseDir/wait/until-next-block.sh

export DATUM_PREFIX=1

echo Second Bid 2
$baseDir/happy-path/bid-2-tx.sh
$baseDir/wait/until-next-block.sh

endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime1))
sleepTime=$((805 - $elapsedTime))
sleep $sleepTime

echo Failed Double Close
detected=false

"$baseDir/failure-cases/double-close-tx.sh" || {
    detected=true
}

if [ detected == false ]; then
  exit 1
fi

echo Success!
