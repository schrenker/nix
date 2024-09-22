{ inputs, lib, pkgs, ... }: {
  home.homeDirectory = "/home/sebastian";

  home.packages = with pkgs; [
    emacs29-pgtk
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    gitleaks
    docker
    syncthing
    shfmt
    yamllint
  ];

  programs.fish.shellAliases.sw =
    "home-manager switch --flake ~/.config/nix#WSL2";

  home.file = {
    ".ssh/git".source = ./secrets/git;
    ".ssh/config".source = ./secrets/ssh_config;
  };
}
