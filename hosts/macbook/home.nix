{ pkgs, ... }: {

  home.homeDirectory = "/Users/sebastian";

  home.packages = with pkgs; [
    alt-tab-macos
    coreutils-prefixed
    docker-client
    ice-bar
    libtool
    numi
    pinentry_mac
    pngpaste
    raycast
    utm
  ];

  programs.fish.shellAliases.sw = "darwin-rebuild switch --flake ~/.config/nix";

  home.file = {
    ".ssh/git".source = ./secrets/git;
    ".ssh/config".source = ./secrets/ssh_config;
    ".ssh/default".source = ./secrets/default;
    ".config/lulublock.txt".source = ./dotfiles/lulublock.txt;
    ".config/karabiner/karabiner.json".source = ./dotfiles/karabiner/karabiner.json;
    ".config/karabiner/assets/".source = ./dotfiles/karabiner/assets;
    ".config/ghostty".source = ./dotfiles/ghostty;
  };
}
