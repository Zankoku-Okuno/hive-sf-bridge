#!/bin/bash
set -e

here="$(dirname "$0")"

if [ -z "$1" ]; then
  echo >&2 "usage: $0 <hive case number>"
  exit 1
else
  hiveCaseId="$1"
fi


yq -j ea '{"data": select(fileIndex == 0)} * select(fileIndex == 1)' \
  <($here/hive-curl.sh "$hiveCaseId") \
  "$here/../SECRETS/hive-responder-template.json"
