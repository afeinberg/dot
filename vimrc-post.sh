#!/usr/bin/env bash

pushd $HOME

# Set up Vundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

vim +PluginInstall +qall

# Set up cscope
mkdir -p $HOME/.vim/plugin

pushd $HOME/.vim/plugin
curl -LSso cscope_maps.vim http://cscope.sourceforge.net/cscope_maps.vim
popd

popd
