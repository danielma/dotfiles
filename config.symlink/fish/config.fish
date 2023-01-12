# Moving around

zoxide init fish --cmd c | source

# Prompt

set -g fish_prompt_pwd_dir_length 0

# Other

set --export XDG_CONFIG_HOME ~/.config

# FZF

set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=90% --preview-window=wrap --marker="*" --color 16,bg+:18,fg+:7,hl+:2,hl:2,pointer:6,prompt:6,info:5,border:2'
set --export FZF_DEFAULT_COMMAND 'rg --files'

fzf_configure_bindings --directory=\ct \
  --git_log= \
  --git_status=

# RG

set --export RIPGREP_CONFIG_PATH ~/.dotfiles/ripgrep/config

# Work

source ~/.dotfiles/config.symlink/fish/work.fish

# Dev

source ~/.asdf/asdf.fish

set -q EDITOR; or set --export EDITOR "emacsclient -t"

# set --export EDITOR 'nvim'

# Path

fish_add_path ~/.dotfiles/bin
fish_add_path ~/.cargo/bin