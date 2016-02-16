#!/bin/bash

source "/Users/danielma/.localrc"
export PATH='/usr/local/bin:/usr/bin:$PATH'

RAW_PULLS=$(curl -s -H "Authorization: token $PERSONAL_GITHUB_API_TOKEN" "https://api.github.com/orgs/ministrycentered/issues")
PR_OUTPUT=$(echo "$RAW_PULLS" | jq -r 'map(select(.pull_request.html_url)) | group_by(.labels[].name == "+2") | reverse | .[] | map("\(.title) | href=\"\(.pull_request.html_url)\"") | [. + ["---"]] | flatten | join("\n")')
PR_COUNT=$(($(echo "$PR_OUTPUT" | sed '/---/q' | wc -l) - 1))

echo "PRs: $PR_COUNT"
echo "---"
echo "$PR_OUTPUT"
