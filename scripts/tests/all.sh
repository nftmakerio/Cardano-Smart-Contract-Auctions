set -eux
thisDir=$(dirname "$0")
baseDir=$thisDir/../

$thisDir/start-bid1-bid2-close.sh
$thisDir/start-close-with-no-bids.sh
