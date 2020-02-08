#!/bin/bash

# Remove all dotfiles from the home directory if present.
echo Removing any existing dotfiles from your home directory.
rm -rf ~/.zshrc ~/.spacemacs ~/.config/nvim ~/.emacs.d

# Initialize symlinks.
echo Creating symlinks in your home directory that point to this dotfiles repository.
ln -s "$PWD/spacemacs" ~/.spacemacs
ln -s "$PWD/zshrc" ~/.zshrc
ln -s "$PWD/nvim" ~/.config/nvim
ln -s "$PWD/emacs" ~/.emacs.d

# Finished.
echo Dotfiles installation complete.
