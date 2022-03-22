# Moving around

zoxide init fish --cmd c | source

# Prompt

set -g fish_prompt_pwd_dir_length 0

# Other

set --export XDG_CONFIG_HOME ~/.config

# FZF

fzf_configure_bindings --directory=\ct \
  --git_log= \
  --git_status=

set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*"'
set --export FZF_DEFAULT_COMMAND 'rg --files'

# Work

source ~/.dotfiles/config.symlink/fish/work.fish

# Dev

source ~/.asdf/asdf.fish

set --export EDITOR 'nvim'

# Path

fish_add_path ~/.dotfiles/bin