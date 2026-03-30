#!/bin/sh
#
# Claude Code
#
# Sets up Claude Code: installs CLI, symlinks commands and global CLAUDE.md

CLAUDE_DIR="$HOME/.claude"
CLAUDE_SRC="$(cd "$(dirname "$0")" && pwd)"
COMMANDS_SRC="$CLAUDE_SRC/commands"
DOTFILES_DIR="$(dirname "$CLAUDE_SRC")"

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

# Symlink ~/.claude/CLAUDE.md so Claude Code uses our README as the global prompt
if [ -L "$CLAUDE_DIR/CLAUDE.md" ]; then
  echo "  CLAUDE.md already linked"
elif [ -f "$CLAUDE_DIR/CLAUDE.md" ]; then
  echo "  ~/.claude/CLAUDE.md already exists (not a symlink), skipping"
else
  ln -s "$CLAUDE_SRC/README.md" "$CLAUDE_DIR/CLAUDE.md"
  echo "  Linked CLAUDE.md"
fi

exit 0
