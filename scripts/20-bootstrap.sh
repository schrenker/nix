#!/usr/bin/env bash

echo "Backing up /etc/nix/nix.conf and shells"
sudo mv /etc/nix/nix.conf /etc/nix/.nix-darwin.bkp.nix.conf
sudo mv /etc/shells /etc/shells.bkp

echo "Linking certificates for the build time"
sudo rm /etc/ssl/certs/ca-certificates.crt
sudo ln -s /nix/var/nix/profiles/default/etc/ssl/certs/ca-bundle.crt /etc/ssl/certs/ca-certificates.crt

cd ~/.config/nix

echo "Building flake"
nix build .#darwinConfigurations.Macbook.system

echo "Activate first environment"
sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
sudo unlink /etc/ssl/certs/ca-certificates.crt
./result/sw/bin/darwin-rebuild switch --flake .#Macbook

echo "Changing current user login shell to fish"
echo "/etc/profiles/per-user/sebastian/bin/fish" | sudo tee -a /etc/shells
chsh -s /etc/profiles/per-user/sebastian/bin/fish

echo "Activate the environment from PATH"
darwin-rebuild switch --flake .#Macbook
