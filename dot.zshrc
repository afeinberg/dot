# -*-shell-script-*-
# vim: ft=zsh

# Custom functions
gitAddModified() {
 git status|grep modified|awk -F ' ' '{print $2}'|xargs git add
}

# OMZ
export ZSH="${HOME}/.oh-my-zsh"
ZSH_THEME="robbyrussell"
zstyle ':omz:update' mode reminder  # just remind me to update when it's time
plugins=(git fzf zsh-dircolors-solarized) # run lssolarized then setupsolarized to use

test -e "${HOME}/.zshrc.local" && source "${HOME}/.zshrc.local"

source $ZSH/oh-my-zsh.sh
unalias gam
#unalias gg
alias gam=gitAddModified
alias ls='ls --color -F'

alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'

export PY3_SITE_PACKAGES=$(pip3 2>/dev/null show powerline-status|awk -F ': ' '$1 == "Location" {print $2}')

# Misc tools
test -r "${HOME}/.opam/opam-init/init.zsh" && . "${HOME}/.opam/opam-init/init.zsh" > /dev/null 2> /dev/null || true
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "${HOME}/.sdkman/bin/sdkman-init.sh" ]] && source "${HOME}/.sdkman/bin/sdkman-init.sh"
