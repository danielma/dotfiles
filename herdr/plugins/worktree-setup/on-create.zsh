#!/usr/bin/env zsh
# Runs repos/<repo-name>.zsh inside a freshly created Herdr worktree, if one
# exists for that repo. <repo-name> is the directory name of the main checkout
# (e.g. ChurchCenterApp), not the worktree's generated name.

set -euo pipefail

# Herdr's server may not have Homebrew on PATH
export PATH="/opt/homebrew/bin:$PATH"

worktree_path=$(jq -r '.worktree.path // empty' <<< "$HERDR_PLUGIN_EVENT_JSON")

if [[ -z "$worktree_path" ]]; then
  echo "worktree-setup: no worktree.path in event payload" >&2
  echo "$HERDR_PLUGIN_EVENT_JSON" >&2
  exit 1
else
  common_dir=$(git -C "$worktree_path" rev-parse --path-format=absolute --git-common-dir)
  repo_name=${${common_dir:h}:t}
  setup_script="$HERDR_PLUGIN_ROOT/repos/$repo_name.zsh"

  if [[ -f "$setup_script" ]]; then
    echo "worktree-setup: running $repo_name setup in $worktree_path"
    cd "$worktree_path"
    zsh "$setup_script"
  else
    echo "worktree-setup: no setup script for $repo_name"
  fi
fi
