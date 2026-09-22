autoload -Uz add-zsh-hook vcs_info

setopt prompt_subst
zstyle ':vcs_info:git:*' formats ' %F{green}%b%f'
zstyle ':vcs_info:git:*' actionformats ' %F{yellow}%b|%a%f'

typeset -g _prompt_host=''
typeset -g _prompt_status=''
typeset -g _prompt_dirty=''

if [[ -n ${SSH_CONNECTION:-}${SSH_TTY:-} ]]; then
  _prompt_host='%F{magenta}%m%f:'
fi

_dotfiles_prompt_precmd() {
  local previous_status=$?

  _prompt_status=''
  (( previous_status != 0 )) && _prompt_status=" %F{red}[$previous_status]%f"

  vcs_info
  _prompt_dirty=''
  if [[ -n $vcs_info_msg_0_ ]] &&
      command git status --porcelain --untracked-files=normal 2>/dev/null | command grep -q .; then
    _prompt_dirty=' %F{red}*%f'
  fi
}

add-zsh-hook precmd _dotfiles_prompt_precmd

PROMPT='${_prompt_host}%F{blue}%~%f${vcs_info_msg_0_}${_prompt_dirty}${_prompt_status}
%# '
