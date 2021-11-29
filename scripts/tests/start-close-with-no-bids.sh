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
$baseDir/happy-path/lock-tx.sh 1
$baseDir/wait/until-next-block.sh

$baseDir/wait/until-next-block.sh

echo Send NFT Back
$baseDir/happy-path/close-start-expired-tx.sh
