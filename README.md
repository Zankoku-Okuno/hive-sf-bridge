# Hive-Salesforce Bridge

Marshall cases from TheHive to Salesforce.

A json configuration file is at https://github.com/layer-3-communications/l3c-cortex-responders

Test scripts are in `scripts/`, but if you get odd behavoir out of them, trhy setting environment variables to populate any needed apikey/other credentials.

  * `hive-curl <case id>` to get json for a hive case
  * `mock-responder-input <case id>` to form a mock input, as long as you have the secret responder case template file
  * `test-hive-to-es` what it says on the tin, probably borken
