module IPs where

import Prelude

-- | 10 times the same IP
sameIP10Times :: [String]
sameIP10Times = replicate 10 $ head manyIPs

-- | 100 IPs taken from here https://ring.nlnog.net/api/1.0/nodes
manyIPs :: [String]
manyIPs =
  [ "200.132.1.8"
  , "37.49.155.108"
  , "41.60.207.30"
  , "221.121.140.194"
  , "93.93.129.145"
  , "41.191.229.177"
  , "192.122.200.171"
  , "128.199.216.83"
  , "31.204.129.45"
  , "190.103.186.186"
  , "92.54.7.29"
  , "147.28.0.89"
  , "190.210.175.57"
  , "185.54.30.136"
  , "85.200.239.110"
  , "109.247.116.75"
  , "46.30.209.1"
  , "85.113.233.194"
  , "109.205.75.161"
  , "46.20.247.200"
  , "37.128.134.97"
  , "37.148.176.54"
  , "195.176.255.11"
  , "192.96.206.13"
  , "103.6.87.36"
  , "185.86.20.92"
  , "212.83.32.45"
  , "91.208.87.80"
  , "85.31.196.24"
  , "213.168.176.84"
  , "52.10.159.54"
  , "77.247.183.177"
  , "212.45.52.2"
  , "139.162.229.237"
  , "194.187.248.66"
  , "192.42.123.22"
  , "46.227.66.10"
  , "205.173.255.242"
  , "83.96.201.176"
  , "150.254.166.148"
  , "81.94.161.36"
  , "89.163.234.146"
  , "193.150.22.68"
  , "213.5.10.55"
  , "78.128.211.50"
  , "178.236.73.122"
  , "217.115.1.67"
  , "194.169.225.130"
  , "198.129.33.90"
  , "45.56.92.54"
  , "212.103.89.211"
  , "85.118.134.2"
  , "192.99.153.129"
  , "174.137.148.20"
  , "199.189.66.254"
  , "198.54.97.14"
  , "145.100.181.27"
  , "68.64.40.68"
  , "94.237.40.201"
  , "45.63.14.202"
  , "82.219.48.9"
  , "192.157.82.100"
  , "212.98.93.10"
  , "212.114.113.69"
  , "87.238.57.254"
  , "84.88.14.229"
  , "213.95.10.76"
  , "109.71.96.106"
  , "193.17.192.2"
  , "85.222.196.125"
  , "200.238.130.54"
  , "141.105.16.129"
  , "91.237.68.9"
  , "37.48.73.130"
  , "72.34.255.61"
  , "195.234.45.126"
  , "94.76.229.204"
  , "70.36.170.6"
  , "212.83.212.122"
  , "193.1.33.23"
  , "193.189.139.240"
  , "185.120.22.17"
  , "192.30.252.225"
  , "104.236.41.143"
  , "162.223.16.247"
  , "62.210.156.8"
  , "128.127.31.126"
  , "185.134.31.241"
  , "129.143.4.179"
  , "82.195.72.4"
  , "185.214.69.134"
  , "193.166.255.250"
  , "78.33.35.246"
  , "185.145.200.76"
  , "195.157.9.4"
  , "82.148.223.107"
  , "185.22.208.142"
  , "108.60.128.14"
  , "91.212.242.244"
  , "217.151.198.151"
  ]
