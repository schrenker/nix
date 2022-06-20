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

  # create /etc/zshrc that loads the nix-darwin environment
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish.enable = true;
  environment.shells = with pkgs; [ bashInteractive fish zsh ];
  # requires 'chsh -s /run/current-system/sw/bin/fish' after that to set up fish

  # backwards compatibility, please read the changelog before changing
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  users.users.ansible = {
    name = "ansible";
    home = "/Users/ansible";
  };

  home-manager.useUserPackages = true;
  home-manager.users.ansible = ./home.nix;

  homebrew.enable = true;
  homebrew.brews = [];
  homebrew.casks = [];
  homebrew.cleanup = "zap";
  homebrew.extraconfig = ''
  '';
  homebrew.masApps = {};

  fonts.fontDir.enable = true;
  fonts.fonts = with pkgs; [
     recursive
     (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
  ];


  # Add ability to used TouchID for sudo authentication
  # security.pam.enableSudoTouchIdAuth = true;
}
