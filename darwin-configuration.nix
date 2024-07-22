{ pkgs, ... }: {
  # environment.systemPackages = with pkgs;
  #   [
  #     # define packages available on system level for all users
  #   ];

  # auto upgrade nix package and the daemon service
  services.nix-daemon.enable = true;

  # create /etc shell files that loads the nix-darwin environment
  programs.fish.enable = true;
  environment.shells = with pkgs; [ bashInteractive fish zsh ];
  # first time requires 'chsh -s /etc/profiles/per-user/sebastian/bin/fish' after that to set up fish

  environment.variables.EDITOR = "vi";

  # Link nix-darwin flake nixpkgs to system
  environment.postBuild = ''
    ln -sv ${pkgs.path} $out/nixpkgs
  '';

  # backwards compatibility, please read the changelog before changing
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  users.users.sebastian = {
    name = "sebastian";
    home = "/Users/sebastian";
    shell = pkgs.fish;
  };

  homebrew.enable = true;
  homebrew.brews = [
    "aspell"
    "coreutils"
    "editorconfig"
    "gcc"
    "libtool"
    "lima"
    "pngpaste"
    "svn"
  ];
  homebrew.casks = [
    "alt-tab"
    "cloudflare-warp"
    "crossover"
    "font-jetbrains-mono"
    "font-jetbrains-mono-nerd-font"
    "hazeover"
    "iterm2"
    "jordanbaird-ice"
    "karabiner-elements"
    "knockknock"
    "logitech-options"
    "lulu"
    "numi"
    "onecast"
    "raycast"
    "syncthing"
    "synologyassistant"
    "vmware-fusion"
    "zsa-wally"
  ];
  homebrew.taps = [
    "d12frosted/emacs-plus"
    "homebrew/cask-drivers"
    "homebrew/cask-fonts"
    "homebrew/cask-versions"
    "homebrew/services"
  ];

  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.extraConfig = ''
    brew "emacs-plus", args: ["with-dbus", "with-mailutils", "with-no-frame-refocus", "with-xwidgets", "with-imagemagick", "with-native-comp", "with-nobu417-big-sur-icon", "with-poll"]
  '';
  homebrew.masApps = {
    "Amphetamine" = 937984704;
    "Bitwarden" = 1352778147;
    "DuckDuckGo Privacy for Safari" = 1482920575;
    "Equinox" = 1591510203;
    "Hush" = 1544743900;
    "ImageFinder for Safari" = 1514863337;
    "Microsoft Remote Desktop" = 1295203466;
    "Noir" = 1592917505;
    "SponsorBlock for YouTube - Skip Sponsorships" = 1573461917;
    "Wipr" = 1320666476;
    "uBlacklist for Safari" = 1547912640;
  };

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
    sudo rm -f /etc/shells /etc/bashrc /etc/zshrc
  '';
  security.pam.enableSudoTouchIdAuth = true;
}
