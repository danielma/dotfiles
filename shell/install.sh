#!/bin/sh
#
# Shell
#
# Sets fish as the default shell.

FISH_PATH=$(which fish)

if test -z "$FISH_PATH"
then
    echo "  fish not found, skipping shell setup."
    exit 0
fi

# Add fish to allowed shells if not already present
if ! grep -q "$FISH_PATH" /etc/shells
then
    echo "  Adding fish to /etc/shells (requires sudo)."
    echo "$FISH_PATH" | sudo tee -a /etc/shells > /dev/null
fi

# Set fish as default shell if it isn't already
if test "$SHELL" != "$FISH_PATH"
then
    echo "  Setting fish as default shell."
    chsh -s "$FISH_PATH"
fi

exit 0
