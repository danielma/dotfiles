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

fzf_configure_bindings --directory=\ct \
    --git_log= \
    --git_status=

eval (gdircolors -c)

# RG

set --export RIPGREP_CONFIG_PATH ~/.dotfiles/ripgrep/config

# Work

source ~/.dotfiles/config.symlink/fish/work.fish

# Secrets

source ~/.dotfiles/config.symlink/secrets

# Dev

# ASDF configuration code
if test -z $ASDF_DATA_DIR
    set _asdf_shims "$HOME/.asdf/shims"
else
    set _asdf_shims "$ASDF_DATA_DIR/shims"
end

# Do not use fish_add_path (added in Fish 3.2) because it
# potentially changes the order of items in PATH
if not contains $_asdf_shims $PATH
    set -gx --prepend PATH $_asdf_shims
end
set --erase _asdf_shims

# Direnv

direnv hook fish | source
