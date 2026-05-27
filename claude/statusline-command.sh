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

# Daily cost from today's transcript files, priced per model:
#   opus-4-*:   in=$15  out=$75  cache_create=$18.75 cache_read=$1.50
#   haiku-4-*:  in=$0.8 out=$4   cache_create=$1.00  cache_read=$0.08
#   sonnet-4-*: in=$3   out=$15  cache_create=$3.75  cache_read=$0.30
cost_info=""
projects_dir="$HOME/.claude/projects"
if [ -d "$projects_dir" ]; then
    today=$(date +%Y-%m-%d)
    sentinel="/tmp/.claude-cost-sentinel-${today}"

    # Ensure sentinel file exists and is timestamped at midnight today
    if [ ! -f "$sentinel" ]; then
        touch -t "$(date +%Y%m%d)0000" "$sentinel" 2>/dev/null
    fi

    daily_cost=$(find "$projects_dir" -name "*.jsonl" -newer "$sentinel" 2>/dev/null \
        -exec cat {} \; 2>/dev/null | \
        jq -r --arg today "$today" '
            select(
              .type == "assistant"
              and .message.usage != null
              and (.timestamp // "" | startswith($today))
            ) |
            .message.model as $m |
            .message.usage |
            (if   $m | startswith("claude-opus")  then {i:15,  o:75, cc:18.75, cr:1.50}
             elif $m | startswith("claude-haiku") then {i:0.8, o:4,  cc:1.00,  cr:0.08}
             else                                      {i:3,   o:15, cc:3.75,  cr:0.30}
             end) as $p |
            ((.input_tokens // 0) * $p.i +
             (.output_tokens // 0) * $p.o +
             ((.cache_creation_input_tokens // 0) +
              (.cache_creation.ephemeral_5m_input_tokens // 0) +
              (.cache_creation.ephemeral_1h_input_tokens // 0)) * $p.cc +
             (.cache_read_input_tokens // 0) * $p.cr) / 1000000
        ' 2>/dev/null | \
        awk '{s+=$1} END{if(s>0) printf "%.2f", s}')

    if [ -n "$daily_cost" ]; then
        cost_info=$(printf " ${DIM}[\$%s today]${RESET}" "$daily_cost")
    fi
fi

printf "${BOLD_BLUE}%s${RESET}%s%s%s%s" "$cwd_display" "$git_info" "$ctx_info" "$model_info" "$cost_info"
