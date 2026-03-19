#!/bin/sh
#
# Homebrew
#
# This installs some of the common dependencies needed (or at least desired)
# using Homebrew.

if test "$(uname)" = "Darwin"
then
    if test ! $(which brew)
    then
        echo "  Installing Homebrew for you."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi

    echo "  Installing Homebrew packages from Brewfile."
    brew bundle install --global
fi

exit 0
