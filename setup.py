#!/usr/bin/python

from string import Template
import os

PATHS = dict({'dot.emacs':'${HOME}/.emacs', 
              'elisp':'${HOME}/elisp'})

def setup_paths():
    for origin, destination in PATHS.items():
        home = os.environ['HOME']
        dest_real = Template(destination).substitute(HOME=home)
        print origin + " => " + dest_real
        os.symlink(os.path.realpath(origin), dest_real)
if __name__ == "__main__":
    setup_paths()
