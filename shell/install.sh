#!/bin/sh
#
# Shell
#
# Sets zsh as the default shell.

ZSH_PATH=$(which zsh)

if test -z "$ZSH_PATH"
then
    echo "  zsh not found, skipping shell setup."
    exit 0
fi

# Add zsh to allowed shells if not already present
if ! grep -q "$ZSH_PATH" /etc/shells
then
    echo "  Adding zsh to /etc/shells (requires sudo)."
    echo "$ZSH_PATH" | sudo tee -a /etc/shells > /dev/null
fi

# Set zsh as default shell if it isn't already
if test "$SHELL" != "$ZSH_PATH"
then
    echo "  Setting zsh as default shell."
    chsh -s "$ZSH_PATH"
fi

exit 0
