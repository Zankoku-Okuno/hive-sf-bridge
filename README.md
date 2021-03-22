# Hive-Salesforce Bridge

Marshall cases from TheHive to Salesforce.

A json configuration file is at https://github.com/layer-3-communications/l3c-cortex-responders

Test scripts are in `scripts/`, but if you get odd behavoir out of them, trhy setting environment variables to populate any needed apikey/other credentials.

  * `hive-curl <case id>` to get json for a hive case
  * `mock-responder-input <case id>` to form a mock input, as long as you have the secret responder case template file
  * `test-hive-to-es` what it says on the tin, probably borken

## Reusable Stuff

The stuff in `src` is meant to be general purpose.
It includes a port of the official `cortexutils.py` to Haskell (WIP, but enough for cases atm).
It also has some small clients (essentially just authorization flows) for Elastisearch, Salesforce, and TheHive.

## Deployment

Dockerhub looks at this github repo for source changes.
Cortex I think looks at `l3c-cortex-responders` on Dockerhub to know when to update.
