{ pkgs, lib, ... }: {
  # environment.systemPackages = with pkgs;
  #   [
  #     # define packages available on system level for all users
  #   ];

  nix.linux-builder = {
    enable = true;
    ephemeral = true;
    systems = [ "aarch64-linux" "x86_64-linux" ];
    maxJobs = 4;
    config = {
      networking.nameservers = [ "192.168.1.254" "9.9.9.9" ];
      virtualisation = {
        darwin-builder = {
          diskSize = 60 * 1024;
          memorySize = 8 * 1024;
        };
        cores = 6;
      };
    };
  };
  nix.settings.trusted-users = [ "@admin" ];

  nixpkgs.config.allowUnfree = true;

  # create /etc shell files that loads the nix-darwin environment
  programs.fish.enable = true;
  environment.shells = with pkgs; [ bashInteractive fish zsh ];
  # first time requires 'chsh -s /etc/profiles/per-user/sebastian/bin/fish' after that to set up fish

  environment.variables.EDITOR = "vi";

  # Link nix-darwin flake nixpkgs to system
  environment.extraSetup = ''
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
  homebrew.brews = [ "gcc" "colima" ];
  homebrew.casks = [
    "badgeify"
    "balenaetcher"
    "betterdisplay"
    "crossover"
    "font-jetbrains-mono"
    "font-jetbrains-mono-nerd-font"
    "ghostty"
    "hazeover"
    "karabiner-elements"
    "keymapp"
    "logitech-options"
    "lulu"
    "mullvad-vpn"
    "onecast"
    "pcsx2"
    "steam"
    "syncthing-app"
    "vivaldi"
  ];
  homebrew.taps = ["homebrew/services" ];

  homebrew.onActivation.cleanup = "zap";
  homebrew.onActivation.upgrade = true;
  homebrew.onActivation.autoUpdate = true;
  homebrew.masApps = {
    "Amphetamine" = 937984704;
    "Bitwarden" = 1352778147;
    "DuckDuckGo Privacy for Safari" = 1482920575;
    "Equinox" = 1591510203;
    "Hush" = 1544743900;
    "ImageFinder for Safari" = 1514863337;
    "Microsoft Remote Desktop" = 1295203466;
    "Noir" = 1592917505;
    "RYTD" = 6463305263;
    "SponsorBlock for YouTube - Skip Sponsorships" = 1573461917;
    "Wipr" = 1320666476;
    "uBlacklist for Safari" = 1547912640;
  };

  system.primaryUser = "sebastian";

  ids.uids.nixbld = 450;
  ids.gids.nixbld = 450;

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

  networking.applicationFirewall.enable = true;
  networking.applicationFirewall.blockAllIncoming = true;
  networking.applicationFirewall.allowSignedApp = true;
  networking.applicationFirewall.allowSigned = true;
  networking.applicationFirewall.enableStealthMode = true;

  system.defaults.loginwindow.GuestEnabled = false;

  system.defaults.screencapture.location = "~/Pictures/Screenshots";
  # system.activationScripts.preActivation.text = ''
  #   sudo rm -f /etc/shells /etc/bashrc /etc/zshrc
  # '';

  system.defaults.CustomUserPreferences = {
    "org.gnu.Emacs" = { AppleFontSmoothing = 0; };
  };
  security.pam.services.sudo_local.touchIdAuth = true;
}
