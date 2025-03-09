{ pkgs, ... }: {

  home.homeDirectory = "/Users/sebastian";

  home.packages = with pkgs; [
    coreutils-prefixed
    docker-client
    iterm2
    libtool
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
    ".config/iterm2/com.googlecode.iterm2.plist".source =
      ./dotfiles/iterm2/com.googlecode.iterm2.plist;
    "Library/Application Support/iTerm2/Scripts/AutoLaunch/auto_dark_mode.py".source =
      ./dotfiles/iterm2/auto_dark_mode.py;
    ".config/karabiner/karabiner.json".source = ./dotfiles/karabiner/karabiner.json;
    ".config/karabiner/assets/".source = ./dotfiles/karabiner/assets;
    ".config/vivaldi/".source = ./dotfiles/vivaldi;
  };
}
