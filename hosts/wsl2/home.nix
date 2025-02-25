{ pkgs, ... }: {
  home.username = "sebastian";
  home.homeDirectory = "/home/sebastian";

  home.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    docker
    gitleaks
    syncthing
    yamllint
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30-pgtk;
    extraPackages = (epkgs: [ epkgs.treesit-grammars.with-all-grammars ]);
  };

  programs.fish.shellAliases.sw =
    "home-manager switch --flake ~/.config/nix#WSL2";

  home.file = {
    ".ssh/git".source = ./secrets/git;
    ".ssh/config".source = ./secrets/ssh_config;
  };

  home.stateVersion = "23.11";
}
