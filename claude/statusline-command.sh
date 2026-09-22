#!/usr/bin/env bash

input=$(cat)

cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
model=$(echo "$input" | jq -r '.model.display_name // empty')
used_pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')
fast_mode=$(echo "$input" | jq -r '.fast_mode // false')
effort=$(echo "$input" | jq -r '.effort.level // empty')

# Bold blue for cwd (matching fish prompt's set_color -o blue)
BOLD_BLUE=$'\033[1;34m'
RESET=$'\033[0m'
DIM=$'\033[2m'
GREEN=$'\033[32m'
YELLOW=$'\033[33m'
RED=$'\033[1;31m'

# Shorten home directory
cwd_display="${cwd/#$HOME/~}"

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

# Context usage, color-coded so it's noticeable before a compaction hits
ctx_info=""
if [ -n "$used_pct" ]; then
    ctx_color="$GREEN"
    if awk -v p="$used_pct" 'BEGIN{exit !(p>=80)}'; then
        ctx_color="$RED"
    elif awk -v p="$used_pct" 'BEGIN{exit !(p>=50)}'; then
        ctx_color="$YELLOW"
    fi
    ctx_info=$(printf " ${DIM}[ctx: ${ctx_color}%.0f%%${DIM}]${RESET}" "$used_pct")
fi

# Model info
model_info=""
if [ -n "$model" ]; then
    model_info=" ${DIM}${model}${RESET}"
fi

# Fast mode / reasoning effort indicator
mode_info=""
if [ "$fast_mode" = "true" ]; then
    mode_info="${mode_info} ${DIM}[fast]${RESET}"
fi
if [ -n "$effort" ]; then
    mode_info="${mode_info} ${DIM}[effort: ${effort}]${RESET}"
fi

printf "${BOLD_BLUE}%s${RESET}%s%s%s%s" "$cwd_display" "$git_info" "$ctx_info" "$model_info" "$mode_info"
