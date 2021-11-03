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
	ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
	# Install homebrew packages
	brew install grc coreutils spark
  fi
fi


exit 0
