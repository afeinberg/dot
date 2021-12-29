# vim: ft=zsh

gitAddModified() {
 git status|awk -F ':' '/modified/ {print $2}'|xargs git add
}
alias gam=gitAddModified
alias ls='ls -G'

test -r "${HOME}/.opam/opam-init/init.zsh" && . "${HOME}/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null || true
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

# OMZ
export ZSH="${HOME}/.oh-my-zsh"
ZSH_THEME="robbyrussell"
zstyle ':omz:update' mode reminder  # just remind me to update when it's time
plugins=(git)
# Allow overriding OMZ configs before loading oh-my-zsh.sh
test -e "${HOME}/.zshrc.local" && source "${HOME}/.zshrc.local"
source $ZSH/oh-my-zsh.sh

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]] && source "${HOME}/.sdkman/bin/sdkman-init.sh"
