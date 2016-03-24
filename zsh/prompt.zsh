autoload colors && colors
# cheers, @ehrenmurdick
# http://github.com/ehrenmurdick/config/blob/master/zsh/prompt.zsh

if (( $+commands[git] ))
then
  git="$commands[git]"
else
  git="/usr/bin/git"
fi

git_branch() {
  echo $($git symbolic-ref HEAD 2>/dev/null | awk -F/ {'print $NF'})
}

git_dirty() {
  if $(! $git status -s &> /dev/null)
  then
    echo ""
  else
    if [[ $($git status --porcelain) == "" ]]
    then
      echo "on $(git_project) %{$fg_bold[green]%}$(git_prompt_info)%{$reset_color%}"
    else
      echo "on $(git_project) %{$fg_bold[red]%}$(git_prompt_info)%{$reset_color%}"
    fi
  fi
}

git_prompt_info () {
 ref=$($git symbolic-ref HEAD 2>/dev/null) || return
# echo "(%{\e[0;33m%}${ref#refs/heads/}%{\e[0m%})"
  echo "${ref#refs/heads/}"
}

unpushed () {
  $git cherry -v @{upstream} 2>/dev/null
}

need_push () {
  if [[ $(unpushed) == "" ]]
  then
    echo " "
  else
    echo " %{$fg_bold[magenta]%}⇧%{$reset_color%} "
  fi
}

project() {
  git remote -v 2>/dev/null | head -n1 | awk '{print $2}' | sed 's/.*\///' | sed 's/\.git//' || ""
}

git_project() {
  if ! [[ -z "$(project)" ]]
  then
    echo "%{$fg_bold[yellow]%}$(project)%{$reset_color%}"
  else
    echo ""
  fi
}

directory_name() {
  echo "%{$fg_bold[cyan]%}%~%{$reset_color%}"
}

prompt_time() {
  echo "%{$fg[magenta]%}%D{%r}%{$reset_color%}"
}

export PROMPT=$'\n$(directory_name) $(git_dirty)$(need_push)\n\u26A1 '
set_prompt () {
  export RPROMPT="$(prompt_time)"
}

precmd() {
  title "zsh" "%m" "%55<...<%~"
  set_prompt
}
