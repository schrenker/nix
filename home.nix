{ config, pkgs, lib, ... }:
let
  unstable = import <unstable> { config = { allowUnfree = true; }; };
in
{
  home.stateVersion = "22.05";
  home.username = "sebastian";
  home.homeDirectory = "/Users/sebastian";

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    arping
    aspell
    fish
    nix-direnv
    git
    jq
    kubectl
    neofetch
    podman
    shellcheck
    terraform
    tmux
    yamllint
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.fish = {
    enable = true;

    shellInit = builtins.readFile ./dotfiles/config.fish;

    shellAliases = {
      config = "git --git-dir=$HOME/.cfg/ --work-tree=$HOME";
      wget = "wget --hsts-file ~/.config/wget/wget-hsts";
      vi = "TERM=xterm-new emacsclient -nw";
      nQ = "networkQuality";
      fixproj = "rm ~/.config/emacs/.local/cache/treemacs-persist";
    };

    plugins = [
      {
        name = "plugin-bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "f969c618301163273d0a03d002614d9a81952c1e";
          sha256 = "A8ydBX4LORk+nutjHurqNNWFmW6LIiBPQcxS3x4nbeQ=";
        };
      }
      {
        name = "plugin-foreign-env";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-foreign-env";
          rev = "b3dd471bcc885b597c3922e4de836e06415e52dd";
          sha256 = "3h03WQrBZmTXZLkQh1oVyhv6zlyYsSDS7HTHr+7WjY8=";
        };
      }
      {
        name = "theme-cmorrell.com";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "theme-cmorrell.com";
          rev = "de213619ec87a3434781dd6ea8b47e219e46fb11";
          sha256 = "3h03WQrBZmTXZLkQh1oVyhv6zlyYsSDS7HTHr+7WjY8=";
        };
      }
    ];
  };

  programs.git = {
    enable = true;

    userEmail = "sebastian@zawadzki.tech";
    userName = "Sebastian Zawadzki";

    extraConfig = {
      init.defaultBranch = "master";
    };

    ignores = [
      ".DS_Store"
    ];
  };

  home.file = {
    ".gnupg/gpg-agent.conf".source = config.lib.file.mkOutOfStoreSymlink ./dotfiles/gpg-agent.conf;
  };
}
