#!/usr/bin/env python3

"""Sets up my dotfiles"""

from string import Template
from typing import Optional

import os
import subprocess
import shutil

PATHS = {'dot.emacs': '${HOME}/.emacs',
         'dot.tmux.conf': '${HOME}/.tmux.conf',
         'oh-my-zsh': '${HOME}/.oh-my-zsh',
         'dot.vimrc': '${HOME}/.vimrc',
         'dot.xmonad': '${HOME}/.xmonad',
         'dot.zshrc': '${HOME}/.zshrc',
         'dot.Xdefaults': '${HOME}/.Xdefaults',
         'elisp': '${HOME}/elisp'}

PLUGINS = {'ale': "git clone --depth 1 https://github.com/dense-analysis/ale.git ~/.vim/pack/git-plugins/start/ale",
           'zsh-dircolors-solarized': "git clone --recursive https://github.com/joel-porquet/zsh-dircolors-solarized ~/.oh-my-zsh/custom/plugins/zsh-dircolors-solarized"}

HOME = os.environ['HOME']


def _powershell_profile_path() -> Optional[str]:
    ps_exe = shutil.which('powershell')
    if ps_exe is None:
        return None
    return subprocess.run(
        [ps_exe, os.path.join('t', 'GetPSProfilePath.ps1')],
        stdout=subprocess.PIPE
    ).stdout.decode("utf-8").rstrip()


def _parse_and_copy(src: str, dest_tmpl: str):
    dest_real = Template(dest_tmpl).substitute(HOME=HOME)
    if os.path.lexists(dest_real):
        print(dest_real + " already exists, skipping")
        return

    print(src + " => " + dest_real)
    os.symlink(os.path.realpath(src), dest_real)


def setup_paths():
    """Creates symlinks"""

    paths = PATHS
    ps_profile_path = _powershell_profile_path()
    if ps_profile_path is not None:
        paths['dot.powershell.ps1'] = ps_profile_path

    for origin, destination in PATHS.items():
        if isinstance(destination, list):
            for dest in destination:
                _parse_and_copy(origin, dest)
        else:
            _parse_and_copy(origin, destination)


def install_plugins():
    """Install extra plugins"""
    for plugin_name, install_cmd in PLUGINS.items():
        install_return_code = os.system(install_cmd)
        if install_return_code != 0:
            print("error code was: %d" % install_return_code)


if __name__ == "__main__":
    setup_paths()
    install_plugins()
