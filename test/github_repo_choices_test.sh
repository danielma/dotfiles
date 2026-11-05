#!/usr/bin/env bash

set -euo pipefail

SCRIPT="$(cd "$(dirname "$0")/.." && pwd)/bin/github-repo-choices"
TEST_DIR="$(mktemp -d)"
MOCK_BIN="$TEST_DIR/bin"
CACHE_ROOT="$TEST_DIR/cache"
CALL_LOG="$TEST_DIR/gh-calls"
REFRESH_LOG="$TEST_DIR/refresh.log"

cleanup() {
  rm -rf "$TEST_DIR"
}

fail() {
  printf 'FAIL: %s\n' "$1" >&2
  [ ! -f "$REFRESH_LOG" ] || cat "$REFRESH_LOG" >&2
  exit 1
}

wait_for() {
  attempts=50

  until "$@"; do
    attempts=$((attempts - 1))
    [ "$attempts" -gt 0 ] || return 1
    sleep 0.1
  done
}

cache_contains() {
  jq -e --arg name "$1" '.[0].text == $name' "$CACHE_ROOT/github-repo-choices/repos.json" >/dev/null
}

refresh_finished_after() {
  previous_call_count="$1"
  current_call_count="$(wc -l < "$CALL_LOG" | tr -d ' ')"
  [ "$current_call_count" -gt "$previous_call_count" ] \
    && [ ! -d "$CACHE_ROOT/github-repo-choices/refresh.lock" ]
}

mkdir -p "$MOCK_BIN"
cat > "$MOCK_BIN/gh" <<'EOF'
#!/usr/bin/env bash

set -eu

printf '%s\n' "$*" >> "$GH_CALL_LOG"
[ "${GH_FAIL:-0}" -eq 0 ] || exit 1

case "$*" in
  'api user --jq .login')
    printf 'octocat\n'
    ;;
  'api user/orgs --paginate --jq .[].login')
    ;;
  'repo list octocat --limit 1000 --json nameWithOwner,url,description,pushedAt')
    jq -n --arg name "${GH_REPO_NAME:-octocat/old}" '[{
      nameWithOwner: $name,
      url: ("https://github.com/" + $name),
      description: "A repository",
      pushedAt: "2026-01-01T00:00:00Z"
    }]'
    ;;
  *)
    printf 'Unexpected gh arguments: %s\n' "$*" >&2
    exit 1
    ;;
esac
EOF
chmod +x "$MOCK_BIN/gh"

run_script() {
  env \
    PATH="$MOCK_BIN:$PATH" \
    XDG_CACHE_HOME="$CACHE_ROOT" \
    GH_CALL_LOG="$CALL_LOG" \
    GITHUB_REPO_CHOOSER_REFRESH_LOG="$REFRESH_LOG" \
    GITHUB_REPO_CHOOSER_CACHE_TTL_SECONDS="${CACHE_TTL:-14400}" \
    GH_REPO_NAME="${GH_REPO_NAME:-octocat/old}" \
    GH_FAIL="${GH_FAIL:-0}" \
    "$SCRIPT" "$@"
}

initial_results="$(run_script)"
[ "$(printf '%s' "$initial_results" | jq -r '.[0].text')" = 'octocat/old' ] || fail 'initial refresh did not return GitHub data'
initial_call_count="$(wc -l < "$CALL_LOG" | tr -d ' ')"

cached_results="$(run_script)"
[ "$cached_results" = "$initial_results" ] || fail 'fresh cache changed results'
[ "$(wc -l < "$CALL_LOG" | tr -d ' ')" = "$initial_call_count" ] || fail 'fresh cache called GitHub'

touch -t 202001010000 "$CACHE_ROOT/github-repo-choices/repos.json"
touch -t 202001010000 "$CACHE_ROOT/github-repo-choices/last-refresh-attempt"
GH_REPO_NAME='octocat/new'
stale_results="$(run_script)"
unset GH_REPO_NAME
[ "$(printf '%s' "$stale_results" | jq -r '.[0].text')" = 'octocat/old' ] || fail 'stale cache was not returned immediately'
wait_for cache_contains 'octocat/new' || fail 'background refresh did not update the cache'

touch -t 202001010000 "$CACHE_ROOT/github-repo-choices/repos.json"
touch -t 202001010000 "$CACHE_ROOT/github-repo-choices/last-refresh-attempt"
call_count_before_failure="$(wc -l < "$CALL_LOG" | tr -d ' ')"
GH_FAIL=1 run_script >/dev/null
wait_for refresh_finished_after "$call_count_before_failure" || fail 'failed refresh did not finish'
cache_contains 'octocat/new' || fail 'failed refresh replaced the last successful cache'
failed_refresh_call_count="$(wc -l < "$CALL_LOG" | tr -d ' ')"

alfred_results="$(run_script --format=alfred)"
printf '%s' "$alfred_results" | jq -e '
  (.cache | not)
  and .items[0].title == "octocat/new"
  and .items[0].arg == "https://github.com/octocat/new"
' >/dev/null || fail 'Alfred output was not formatted correctly'
[ "$(wc -l < "$CALL_LOG" | tr -d ' ')" = "$failed_refresh_call_count" ] || fail 'failed refresh was retried before the interval elapsed'

printf 'PASS: github-repo-choices cache behavior\n'
