{ config, pkgs, lib, ... }:
{
  imports = [
    <home-manager/nix-darwin>
  ];

  environment.systemPackages = with pkgs; [
    # define packages available on system level for all users
  ];

  # auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  # create /etc shell files that loads the nix-darwin environment
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish.enable = true;
  environment.shells = with pkgs; [ bashInteractive fish zsh ];
  # first time requires 'chsh -s /run/current-system/sw/bin/fish' after that to set up fish

  # backwards compatibility, please read the changelog before changing
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  users.users.sebastian = {
    name = "sebastian";
    home = "/Users/sebastian";
  };

  home-manager.useUserPackages = true;
  home-manager.users.sebastian = ./home.nix;

  homebrew.enable = true;
  homebrew.brews = [
    "coreutils"
    "editorconfig"
  ];
  homebrew.casks = [
    "alt-tab"
    "bartender"
    "font-jetbrains-mono"
    "hazeover"
    "karabiner-elements"
    "logitech-options"
    "lulu"
    "numi"
    "podman-desktop"
    "raspberry-pi-imager"
    "raycast"
    "synologyassistant"
    "utm"
    "wireshark"
    "zsa-wally"
  ];
  homebrew.taps = [
    "d12frosted/emacs-plus"
    "homebrew/cask"
    "homebrew/cask-versions"
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
  ];
  homebrew.cleanup = "zap";
  homebrew.extraConfig = ''
  brew "emacs-plus", args: ["with-imagemagick", "with-no-frame-refocus", "with-native-comp", "with-nobu417-big-sur-icon", "with-xwidgets"]
  '';
  homebrew.masApps = {};

  fonts.fontDir.enable = true;
  # fonts.fonts = with pkgs; [
  #    recursive
  #    (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  # ];
  system.defaults.finder._FXShowPosixPathInTitle = true;
  system.defaults.finder.ShowStatusBar = true;
  system.defaults.finder.FXPreferredViewStyle = "clmv";
  system.defaults.finder.AppleShowAllExtensions = true;

  system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = true;

  system.defaults.dock.autohide = true;
  system.defaults.dock.autohide-delay = "0.75";
  system.defaults.dock.autohide-time-modifier = "0";
  system.defaults.dock.mineffect = "suck";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.orientation = "left";
  system.defaults.dock.show-recents = false;
  system.defaults.dock.static-only = true;
  system.defaults.dock.tilesize = 32;
  system.defaults.dock.wvous-tl-corner = 1;
  system.defaults.dock.wvous-bl-corner = 1;
  system.defaults.dock.wvous-tr-corner = 1;
  system.defaults.dock.wvous-br-corner = 1;

  system.defaults.alf.globalstate = 1;
  system.defaults.alf.allowsignedenabled = 1;
  system.defaults.alf.allowdownloadsignedenabled = 1;
  system.defaults.alf.stealthenabled = 1;

  system.defaults.loginwindow.GuestEnabled = false;

  system.defaults.screencapture.location = "$HOME/Pictures/Screenshots";
}
