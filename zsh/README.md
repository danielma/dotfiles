# Zsh configuration

Zsh is the login and interactive shell. This directory contains its environment,
completion, prompt, keybinding, and work configuration.

The bootstrap script links `zshenv.symlink` and `zshrc.symlink` to `~/.zshenv`
and `~/.zshrc`. The interactive configuration explicitly loads `work.zsh`,
`completion.zsh`, and `prompt.zsh`; adding another `*.zsh` file does not load it
automatically. `work.zsh` imports the local `~/pco-box/env.sh` environment when
it is available.

## Install

Install the declared Homebrew dependencies and refresh the dotfile symlinks:

```sh
cd ~/.dotfiles
brew bundle --file=config.symlink/homebrew/Brewfile
script/bootstrap
```

The bootstrap installer makes zsh the login shell. To do that manually, make
sure it is listed in `/etc/shells`, then run:

```sh
command -v zsh
grep -Fx "$(command -v zsh)" /etc/shells
chsh -s "$(command -v zsh)"
```

A new login session is required before a login-shell change takes effect.
