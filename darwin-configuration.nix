{ config, pkgs, lib, ... }: {
  imports = [ <home-manager/nix-darwin> ];

  environment.systemPackages = with pkgs;
    [
      # define packages available on system level for all users
    ];

  # auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  # create /etc shell files that loads the nix-darwin environment
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish.enable = true;
  environment.shells = with pkgs; [ bashInteractive fish zsh ];
  # first time requires 'chsh -s /etc/profiles/per-user/sebastian/bin/fish' after that to set up fish

  environment.variables.EDITOR = "vi";

  # backwards compatibility, please read the changelog before changing
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  users.users.sebastian = {
    name = "sebastian";
    home = "/Users/sebastian";
    shell = pkgs.fish;
  };

  home-manager.useUserPackages = true;
  home-manager.users.sebastian = ./home.nix;

  homebrew.enable = true;
  homebrew.brews = [
    "aspell"
    "coreutils"
    "editorconfig"
    "gcc"
    "pngpaste"
    "podman"
    "podman-compose"
    "svn"
  ];
  homebrew.casks = [
    "alt-tab"
    "bartender"
    "cloudflare-warp"
    "crossover"
    "font-jetbrains-mono"
    "font-jetbrains-mono-nerd-font"
    "hazeover"
    "karabiner-elements"
    "logitech-options"
    "lulu"
    "numi"
    "onecast"
    "raycast"
    "syncthing"
    "synologyassistant"
    "zsa-wally"
  ];
  homebrew.taps = [
    "homebrew/cask"
    "homebrew/cask-versions"
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
    "homebrew/services"
    "d12frosted/emacs-plus"
  ];
  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.extraConfig = ''
    brew "emacs-plus@29", args: ["with-dbus", "with-mailutils", "with-no-frame-refocus", "with-xwidgets", "with-imagemagick", "with-native-comp", "with-nobu417-big-sur-icon", "with-poll"]
  '';
  homebrew.masApps = {
    "Amphetamine" = 937984704;
    "Bitwarden" = 1352778147;
    "DuckDuckGo Privacy for Safari" = 1482920575;
    "Equinox" = 1591510203;
    "Flow" = 1423210932;
    "Hush" = 1544743900;
    "ImageFinder for Safari" = 1514863337;
    "Microsoft Remote Desktop" = 1295203466;
    "Microsoft To Do" = 1274495053;
    "Noir" = 1592917505;
    "Sorted" = 1306893526;
    "SponsorBlock for YouTube - Skip Sponsorships" = 1573461917;
    "uBlacklist for Safari" = 1547912640;
    "Wipr" = 1320666476;
  };

  fonts.fontDir.enable = true;

  system.defaults.finder._FXShowPosixPathInTitle = true;
  system.defaults.finder.ShowStatusBar = true;
  system.defaults.finder.FXPreferredViewStyle = "clmv";
  system.defaults.finder.AppleShowAllExtensions = true;

  system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
  system.defaults.NSGlobalDomain.ApplePressAndHoldEnabled = false;

  system.defaults.dock.autohide = true;
  system.defaults.dock.autohide-delay = 0.75;
  system.defaults.dock.autohide-time-modifier = 0.0;
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

  system.defaults.screencapture.location = "~/Pictures/Screenshots";
  system.activationScripts.preActivation.text = ''
    rm -f /etc/shells
  '';
  system.activationScripts.postActivation.text = ''
    if [ ! -d /Applications/Sorted.app ]; then
        mv /Applications/Sorted* /Applications/Sorted.app
    fi
  '';

  security.pam.enableSudoTouchIdAuth = true;
}
