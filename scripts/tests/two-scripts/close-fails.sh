set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../../
startTime=$(date +%s)

$baseDir/wait/until-next-block.sh
# find all the nfts and burn them

echo Mint 1
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

echo Start Auction 1
$baseDir/happy-path/lock-tx.sh 1000000
$baseDir/wait/until-next-block.sh

echo First Bid 1
$baseDir/happy-path/bid-1-tx.sh
$baseDir/wait/until-next-block.sh

echo Second Bid 1
$baseDir/happy-path/bid-2-tx.sh
$baseDir/wait/until-next-block.sh

echo Mint 2
$baseDir/minting/mint-0-policy.sh
$baseDir/wait/until-next-block.sh

echo Start Auction 2
$baseDir/happy-path/lock-tx.sh 500000
$baseDir/wait/until-next-block.sh

echo First Bid 2
$baseDir/happy-path/bid-1-tx.sh
$baseDir/wait/until-next-block.sh

echo Second Bid 2
$baseDir/happy-path/bid-2-tx.sh
$baseDir/wait/until-next-block.sh

endTime=$(date +%s)
elapsedTime=$(($endTime-$startTime))
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
