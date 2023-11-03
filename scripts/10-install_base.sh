#!/usr/bin/env bash

git clone git@github.com:schrenker/nix.git ~/.config/nix

curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install

/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
/opt/homebrew/bin/brew install git-crypt

echo 'Reload shell and launch bootstrap.sh script'
echo 'Remember about unlocking secrets from git before running bootstrap.sh'
