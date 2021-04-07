#!/bin/bash
set -e

if [ -z "$1" ]; then
  echo >&2 "usage: $0 <hive case number>"
  exit 1
else
  hiveCaseId="$1"
fi


if [ -z "$HIVE_APIKEY" ]; then
  echo -n "Hive API key:"
  read -s HIVE_APIKEY
  echo
fi

hiveDomain=hive.noc.layer3com.com
hiveAuth="Authorization: Bearer $HIVE_APIKEY"

curl -k -v "https://${hiveDomain}/api/case/${hiveCaseId}" -H "$hiveAuth"

# get all alerts (takes several seconds)
# curl -k -H "Authorization: Bearer $HIVE_APIKEY" \
#   "https://${HIVE_DOMAIN}/api/alert?range=all" \
#   -o all_alerts.json
