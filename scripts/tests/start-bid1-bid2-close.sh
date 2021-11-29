set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../
startTime=$(date +%s)

$baseDir/wait/until-next-block.sh
# find all the nfts and burn them

echo Mint
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

echo Start Auction
$baseDir/happy-path/lock-tx.sh 500000
$baseDir/wait/until-next-block.sh

echo Early Close Fails
detected=false

"$baseDir/failure-cases/early-close.sh" || {
    detected=true
}

if [ detected == false ]; then
  exit 1
fi

echo First Bid
$baseDir/happy-path/bid-1-tx.sh
$baseDir/wait/until-next-block.sh

echo Second Bid
$baseDir/happy-path/bid-2-tx.sh
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
sleepTime=$((805 - $elapsedTime))
sleep $sleepTime

echo Close
$baseDir/happy-path/close-tx.sh
