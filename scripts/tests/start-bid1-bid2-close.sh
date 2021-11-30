set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
startTime=$(date +%s)

$baseDir/wait/until-next-block.sh
# find all the nfts and burn them

echo Mint
$baseDir/minting/mint-0-policy.sh
sleep 2
$baseDir/wait/until-next-block.sh

echo Start Auction
$baseDir/happy-path/lock-tx.sh 400000 0
sleep 2
$baseDir/wait/until-next-block.sh

echo Early Close Fails
detected=false

"$baseDir/failure-cases/close-too-early-tx.sh" || {
    detected=true
}

if [ detected == false ]; then
  exit 1
fi

echo First Bid
$baseDir/happy-path/bid-1-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

echo Second Bid
$baseDir/happy-path/bid-2-tx.sh
sleep 2
$baseDir/wait/until-next-block.sh

echo Failed Bid
detected=false

"$baseDir/failure-cases/bid-not-high-enough.sh" || {
    detected=true
}

if [ detected == false ]; then
  exit 1
fi

endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime))
sleepTime=$((605 - $elapsedTime))
sleep $sleepTime

echo Close
$baseDir/happy-path/close-tx.sh
