autoload -Uz add-zsh-hook vcs_info

setopt prompt_subst
zstyle ':vcs_info:git:*' formats '%F{green}%b%f'
zstyle ':vcs_info:git:*' actionformats '%F{yellow}%b|%a%f'

typeset -g _prompt_host=''
typeset -g _prompt_status=''
typeset -g _prompt_git=''

if [[ -n ${SSH_CONNECTION:-}${SSH_TTY:-} ]]; then
  _prompt_host='%F{magenta}%m%f:'
fi

_dotfiles_prompt_precmd() {
  local previous_status=$?

  _prompt_status=''
  (( previous_status != 0 )) && _prompt_status=" %F{red}[$previous_status]%f"

  vcs_info
  _prompt_git=''
  if [[ -n $vcs_info_msg_0_ ]]; then
    local dirty=''
    if command git status --porcelain --untracked-files=normal 2>/dev/null | command grep -q .; then
      dirty='%F{red}*%f'
    fi
    _prompt_git=" %F{8}(%f${vcs_info_msg_0_}${dirty}%F{8})%f"
  fi
}

add-zsh-hook precmd _dotfiles_prompt_precmd

# ❯ for normal users, # for root
PROMPT='${_prompt_host}%F{blue}%~%f${_prompt_git}${_prompt_status}
%(!.#.❯) '
