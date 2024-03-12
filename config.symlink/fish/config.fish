# Path

fish_add_path /opt/homebrew/bin
fish_add_path ~/.dotfiles/bin
fish_add_path ~/.cargo/bin
fish_add_path (go env GOPATH)/bin

# Moving around

zoxide init fish --cmd c | source

# Prompt

set -g fish_prompt_pwd_dir_length 0

# Other

set --export XDG_CONFIG_HOME ~/.config

# FZF

set --export FZF_DEFAULT_OPTS '--cycle --layout=reverse --border --height=90% --preview-window=wrap --color 16,bg+:8,fg+:6,hl+:11,hl:11,border:245'
set --export FZF_DEFAULT_COMMAND 'rg --files'

fzf_configure_bindings --directory=\ct \
    --git_log= \
    --git_status=

eval (gdircolors -c)

# RG

set --export RIPGREP_CONFIG_PATH ~/.dotfiles/ripgrep/config

# Work

source ~/.dotfiles/config.symlink/fish/work.fish

# Dev

source ~/.asdf/asdf.fish
