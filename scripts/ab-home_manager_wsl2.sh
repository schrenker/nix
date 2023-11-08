echo '/home/sebastian/.nix-profile/bin/fish' | sudo tee -a /etc/shells

cd ~/.config/nix

nix build .#homeConfigurations.WSL2

./result/activate switch --flake ~/.config/nix#WSL2

home-manager switch --flake ~/.config/nix#WSL2

rm -rf ./result
