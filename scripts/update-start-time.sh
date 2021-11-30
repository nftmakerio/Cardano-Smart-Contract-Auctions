set -eux
thisDir=$(dirname "$0")

now=$(date +%s)
timestamp=$(($now*1000+$1))
prefix=${2:-0}

mkdir -p $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix

sellerPkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/seller-pkh.txt)
marketplacePkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/marketplace-pkh.txt)
royaltyPkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/royalities-pkh.txt)
buyerPkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/buyer-pkh.txt)
buyer1Pkh=$(cat $thisDir/$BLOCKCHAIN_PREFIX/pkhs/buyer1-pkh.txt)

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/start.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": 8000000
    },
    {
      "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
    },
    {
      "bytes": "123456"
    },
    {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 1,
      "fields": [
      ]
    }
  ]
}

EOF

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-1.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": 8000000
    },
    {
      "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
    },
    {
      "bytes": "123456"
    },
        {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyerPkh"
            },
            {
              "int" : 10000000
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/seller-bid-1.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "$sellerPkh"
    },
    {
      "int": $timestamp
    },
    {
      "int": 8000000
    },
    {
      "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
    },
    {
      "bytes": "123456"
    },
    {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "$sellerPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$royaltyPkh"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "$marketplacePkh"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "$buyer1Pkh"
            },
            {
              "int" : 10000000
            }
          ]
        }
      ]
    }
  ]
}

EOF

cat << EOF > $thisDir/$BLOCKCHAIN_PREFIX/datums/$prefix/bid-2.json
{
  "constructor": 0,
  "fields": [
    {
      "bytes": "67614c1b06ddbb100cb6cbe919594cac31771c25530b6c7f28da242b"
    },
    {
      "int": $timestamp
    },
    {
      "int": 8000000
    },
    {
      "bytes": "d6cfdbedd242056674c0e51ead01785497e3a48afbbb146dc72ee1e2"
    },
    {
      "bytes": "123456"
    },
    {
      "map": [
        {
          "v": {
            "int": 900
          },
          "k": {
            "bytes": "67614c1b06ddbb100cb6cbe919594cac31771c25530b6c7f28da242b"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "1d0ab2689eed633f013b347ba5db41919367dfc86d0d74d0a809c3e0"
          }
        },
        {
          "v": {
            "int": 50
          },
          "k": {
            "bytes": "b90c88a3460d723708b5c8a2d8c33951b13f920de8e8ff605480bf2f"
          }
        }
      ]
    },
    {
      "constructor": 0,
      "fields": [
        {
          "constructor": 0,
          "fields": [
            {
              "bytes" : "7beea80f99541c6871da03b3f7606e84ff641baa138a24d3e472678b"
            },
            {
              "int" : 30000000
            }
          ]
        }
      ]
    }
  ]
}

EOF

$thisDir/hash-datums.sh
