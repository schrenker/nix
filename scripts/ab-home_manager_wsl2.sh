cd ~/.config/nix

nix build .#homeConfigurations.WSL2

./result/activate switch --flake ~/.config/nix#WSL2

echo '/home/sebastian/.nix-profile/bin/fish' | sudo tee -a /etc/shells

chsh -s /home/sebastian/.nix-profile/bin/fish

home-manager switch --flake ~/.config/nix#WSL2

rm -rf ./result
