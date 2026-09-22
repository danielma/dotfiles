# Zsh trial

This directory contains a small zsh configuration that can be tried without
changing the login shell. Fish remains installed and configured separately.

The bootstrap script links `zshenv.symlink` and `zshrc.symlink` to `~/.zshenv`
and `~/.zshrc`. The interactive configuration explicitly loads only
`completion.zsh` and `prompt.zsh`; adding another `*.zsh` file does not load it
automatically.

## Try it

Install the declared Homebrew dependencies and refresh the dotfile symlinks:

```sh
cd ~/.dotfiles
brew bundle --file=config.symlink/homebrew/Brewfile
script/bootstrap
zsh
```

Run `exit` to return to Fish, or replace the trial shell immediately with
`exec fish`.

## Adopt it later

Only after deciding to keep zsh, make sure it is listed in `/etc/shells`, then
change the login shell:

```sh
command -v zsh
grep -Fx "$(command -v zsh)" /etc/shells
chsh -s "$(command -v zsh)"
```

Do not run `chsh` merely to try this configuration. A new login session is
required before a login-shell change takes effect.
