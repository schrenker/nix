{ pkgs, ... }: {
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  home.packages = with pkgs; [
    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    docker
    emacs29-pgtk
    gitleaks
    syncthing
    yamllint
  ];

  programs.fish.shellAliases.sw =
    "home-manager switch --flake ~/.config/nix#WSL2";

  home.file = {
    ".ssh/git".source = ./secrets/git;
    ".ssh/config".source = ./secrets/ssh_config;
  };

  home.stateVersion = "23.11";
}
