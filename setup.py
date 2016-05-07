#!/usr/bin/python
"""Sets up my dotfiles"""

from string import Template
import os

PATHS = {'dot.emacs': '${HOME}/.emacs',
         'dot.tmux.conf', '${HOME}/.tmux.conf',
         'dot.vimrc': '${HOME}/.vimrc',
         'dot.Xdefaults': '${HOME}/.Xdefaults',
         'elisp': '${HOME}/elisp'}


def setup_paths():
    """Creates symlinks"""

    for origin, destination in PATHS.items():
        home = os.environ['HOME']
        dest_real = Template(destination).substitute(HOME=home)
        if os.path.lexists(dest_real):
            print dest_real + " already exists, skipping"
            continue

        print origin + " => " + dest_real
        os.symlink(os.path.realpath(origin), dest_real)

if __name__ == "__main__":
    setup_paths()
