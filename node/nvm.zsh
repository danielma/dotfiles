export NVM_DIR=$(brew --prefix)/var/nvm
NVM_SH="$(brew --prefix nvm)/nvm.sh"

[[ -a $NVM_SH ]] && source $NVM_SH
