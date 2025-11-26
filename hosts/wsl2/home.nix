{ pkgs, ... }: {

  home.homeDirectory = "/home/sebastian";

  home.packages = with pkgs; [
    docker
    nerd-fonts.jetbrains-mono
    syncthing
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
}
