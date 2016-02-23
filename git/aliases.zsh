# Use `hub` as our git wrapper:
#   http://defunkt.github.com/hub/
hub_path=$(which hub)
if (( $+commands[hub] ))
then
  alias git=$hub_path
fi

# The rest of my fun git aliases
alias gl='git pull --prune'
alias glog="git log --graph --pretty=format:'%Cred%h%Creset %an: %s - %Creset %C(yellow)%d%Creset %Cgreen(%cr)%Creset' --abbrev-commit --date=relative"
alias gp='git push'
alias gd='git diff'
alias gc='git commit'
alias gca='git commit -a'
alias gco='git checkout'
alias gb='git branch'
alias gs='git status -sb' # upgrade your git if -sb breaks for you. it's fun.
alias ga='git add'
alias gst='git stash'
alias gsl='git stash list'
alias gcb='git branch --merged | grep -v \* | xargs git branch -d'
alias gds='git diff --staged'
alias gr='git rebase'
alias glcb='gl && gcb'

function setup-sanity-check() {
  alias sanity-check="git diff `git rev-parse HEAD`"
}

function staging() {
  CHANGED_LENGTH=$(git status -s | wc -l)
  [[ $CHANGED_LENGTH -ne "0" ]] && echo "NOPE! you have changes" && gs
  [[ $CHANGED_LENGTH -eq "0" ]] && git checkout staging && git fetch origin staging && git reset --hard origin/staging
}

function master() {
  CHANGED_LENGTH=$(git status -s | wc -l)
  [[ $CHANGED_LENGTH -ne "0" ]] && echo "NOPE! you have changes" && gs
  [[ $CHANGED_LENGTH -eq "0" ]] && git checkout master && glcb
}
