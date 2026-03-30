#!/usr/bin/env bash

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
model=$(echo "$input" | jq -r '.model.display_name // empty')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

# Bold blue for cwd (matching fish prompt's set_color -o blue)
BOLD_BLUE=$'\033[1;34m'
RESET=$'\033[0m'
DIM=$'\033[2m'

# Shorten home directory
cwd_display="${cwd/#$HOME/\~}"

# Git branch/status (skip locking issues gracefully)
git_info=""
if git -C "$cwd" rev-parse --git-dir >/dev/null 2>&1; then
    branch=$(git -C "$cwd" symbolic-ref --short HEAD 2>/dev/null || git -C "$cwd" rev-parse --short HEAD 2>/dev/null)
    if [ -n "$branch" ]; then
        dirty=""
        if ! git -C "$cwd" diff --quiet 2>/dev/null || ! git -C "$cwd" diff --cached --quiet 2>/dev/null; then
            dirty="*"
        fi
        untracked=""
        if [ -n "$(git -C "$cwd" ls-files --others --exclude-standard 2>/dev/null | head -1)" ]; then
            untracked="?"
        fi
        git_info=" ${DIM}(${branch}${dirty}${untracked})${RESET}"
    fi
fi

# Context usage
ctx_info=""
if [ -n "$used_pct" ]; then
    ctx_info=$(printf " ${DIM}[ctx: %.0f%%]${RESET}" "$used_pct")
fi

# Model info
model_info=""
if [ -n "$model" ]; then
    model_info=" ${DIM}${model}${RESET}"
fi

printf "${BOLD_BLUE}%s${RESET}%s%s%s" "$cwd_display" "$git_info" "$ctx_info" "$model_info"
