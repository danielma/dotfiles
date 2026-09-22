autoload -Uz compinit
compinit

setopt complete_in_word
setopt complete_aliases

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' menu select
zstyle ':completion:*' insert-tab pending
