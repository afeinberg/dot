#!/usr/bin/env python3

"""Sets up my dotfiles"""

from string import Template
import os


POWERSHELL_PROFILE_PATH = 'C:\Users\${USERNAME}\OneDrive\Desktop\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1'

PATHS = {'dot.emacs': '${HOME}/.emacs',
         'dot.tmux.conf': '${HOME}/.tmux.conf',
         'dot.oh-my-zsh': '${HOME}/.oh-my-zsh',
         'dot.vimrc': '${HOME}/.vimrc',
         'dot.xmonad': '${HOME}/.xmonad',
         'dot.zshrc': '${HOME}/.zshrc',
         'dot.oh_my_zsh': '${HOME}.oh-my-zsh',
         'dot.Xdefaults': ['${HOME}/.Xdefaults', '${HOME}/.Xresources'],
         'elisp': '${HOME}/elisp'}

HOME = os.environ['HOME']

def _parse_and_copy(src, dest_tmpl):
    dest_real = Template(dest_tmpl).substitute(HOME=HOME)
    if os.path.lexists(dest_real):
        print(dest_real + " already exists, skipping")
        return

    print(src + " => " + dest_real)
    os.symlink(os.path.realpath(src), dest_real)

def setup_paths():
    """Creates symlinks"""

    for origin, destination in PATHS.items():
        if isinstance(destination, list):
            for dest in destination:
                _parse_and_copy(origin, dest)
        else:
            _parse_and_copy(origin, destination)

if __name__ == "__main__":
    setup_paths()
