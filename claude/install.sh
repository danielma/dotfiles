#!/bin/sh
#
# Claude Code
#
# Symlinks Claude commands into ~/.claude/commands

CLAUDE_DIR="$HOME/.claude"
COMMANDS_SRC="$(cd "$(dirname "$0")" && pwd)/commands"

if ! command -v claude >/dev/null 2>&1; then
  curl -fsSL https://claude.ai/install.sh | bash
fi

mkdir -p "$CLAUDE_DIR"

if [ -L "$CLAUDE_DIR/commands" ]; then
  echo "  Claude commands already linked"
elif [ -d "$CLAUDE_DIR/commands" ]; then
  echo "  ~/.claude/commands already exists (not a symlink), skipping"
else
  ln -s "$COMMANDS_SRC" "$CLAUDE_DIR/commands"
  echo "  Linked Claude commands"
fi

exit 0
