cd ~/.config/nix

nix build .#homeConfigurations.WSL2.activationPackage

./result/activate switch --flake ~/.config/nix#WSL2

home-manager switch --flake ~/.config/nix#WSL2

echo '/home/sebastian/.local/state/home-manager/gcroots/current-home/home-path/bin/fish' | sudo tee -a /etc/shells

chsh -s /home/sebastian/.local/state/home-manager/gcroots/current-home/home-path/bin/fish

rm -rf ./result
