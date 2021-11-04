# Use `hub` as our git wrapper:
#   http://defunkt.github.com/hub/
hub_path=$(which hub)
if (( $+commands[hub] ))
then
  alias git=$hub_path
fi

# The rest of my fun git aliases
alias gl='git pull --prune'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s -%Creset%C(yellow)%d%Creset %Cgreen(%cd)%Creset' --abbrev-commit --date=human"
alias gp='git push'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gbs='git branch --sort=-committerdate | grep -v "^*" | fzf-tmux | xargs git checkout'
alias gs='git status -sb' # upgrade your git if -sb breaks for you. it's fun.
alias ga='git add'
alias gst='git stash'
alias gsl='git stash list'
alias gcb='git branch --merged | grep -v \* | xargs git branch -d'
alias gds='git diff --staged'
alias gr='git rebase'
alias grm='git fetch origin master && git rebase -i origin/master'
alias glcb='gl && gcb'
alias amend="git commit --amend"

function g {
  if [[ $# > 0 ]]; then
    git $@
  else
    gs
  fi
}

function setup-sanity-check() {
  alias sanity-check="git diff `git rev-parse HEAD`"
}

function gitchanges {
  CHANGED_LENGTH=$(git status -s | wc -l)
  if [[ $CHANGED_LENGTH -ne "0" ]]
  then
    echo true
  else
    echo false
  fi
}

function staging() {
  if $(gitchanges)
  then
    echo "NOPE! you have changes"
    gs
  else
    git checkout staging && git fetch origin staging && git reset --hard origin/staging
  fi
}

function master() {
  if $(gitchanges)
  then
    echo "NOPE! you have changes"
    gs
  else
    git checkout master && glcb
  fi
}

function mergemetostaging() {
  if $(gitchanges)
  then
    echo "NOPE! you have changes"
    gs
    false
  else
    CURRENT_BRANCH=$(git symbolic-ref HEAD | sed 's/refs\/heads\///')
    staging && git merge $CURRENT_BRANCH --no-edit && git push
  fi
}
    

function deploymetostaging() {
  mergemetostaging && stagingdeploy
}

function groot() {
  while [[ -z $(ls -A --color=no | grep \\.git/) ]] {
    cd ..
  }
}
