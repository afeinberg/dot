# vim: ft=zsh

export ZSH="/Users/alex/.oh-my-zsh"

ZSH_THEME="robbyrussell"
zstyle ':omz:update' mode reminder  # just remind me to update when it's time

plugins=(git)

gitAddModified() {
 git status|awk -F ':' '/modified/ {print $2}'|xargs git add
}
alias gam=gitAddModified

source $ZSH/oh-my-zsh.sh

# opam configuration
test -r /Users/alex/.opam/opam-init/init.zsh && . /Users/alex/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

alias ls='ls -G'

gitAddModified() {
 git status|awk -F ':' '/modified/ {print $2}'|xargs git add
}

alias gam=gitAddModified

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
