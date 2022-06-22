#!/usr/bin/env bash

echo "Installing home-manager"
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
nix-channel --add https://nixos.org/channels/nixpkgs-unstable unstable
nix-channel --update

echo "Backing up /etc/nix/nix.conf and shells"
sudo mv /etc/nix/nix.conf /etc/nix/.nix-darwin.bkp.nix.conf
sudo mv /etc/shells /etc/shells.bkp

echo "Installing nix-darwin"
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
./result/bin/darwin-installer

echo "Changing current user login shell to fish"
chsh -s /run/current-system/sw/bin/fish

echo "Checking for emacs configuration"
if [ ! -d "$HOME/.config/doom" ]; then
  echo "Missing. Pulling doom emacs"
  mkdir -p $HOME/.config
  git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
  git clone https://github.com/schrenker/doom.d.git ~/.config/doom
  $HOME/.config/emacs/bin/doom  install
fi

echo "Reload shell to activate fish and run commands:"
echo
echo "For creating environment:"
echo 'darwin-rebuild switch'
echo
echo "For doom installation:"
echo 'doom install'

/opt/homebrew/bin/brew uninstall git-crypt
