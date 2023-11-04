{ lib, config, pkgs, ... }: {
  home.stateVersion = "23.05";
  home.username = "sebastian";
  home.homeDirectory = "/Users/sebastian";

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    arping
    cmake
    d2
    # emacsCustom
    fd
    fish
    git
    git-crypt
    gnupg
    gnutls
    jq
    kind
    kubectl
    kubectx
    libgccjit
    nil
    nix-direnv
    nixfmt
    nodejs
    pinentry_mac
    ripgrep
    tmux
    wget
  ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fish = {
    enable = true;

    shellInit = builtins.readFile ./dotfiles/config.fish;

    shellAliases = {
      wget = "wget --hsts-file ~/.config/wget/wget-hsts";
      nQ = "networkQuality";
      docker = "podman";
      k = "kubectl";
      kx = "kubectx";
      kns = "kubens";
      drs = "darwin-rebuild switch --flake ~/.config/nix";
      nrs = "nixos-rebuild switch --flake ~/.config/nix";
    };

    functions = {
      git_is_repo = {
        description = "Check if directory is a repository";
        body = ''
          test -d .git
            or begin
              set -l info (command git rev-parse --git-dir --is-bare-repository 2>/dev/null)
              and test $info[2] = false
            end
        '';

      };
    };

    plugins = [
      {
        name = "plugin-bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "ec991b8";
          hash = "sha256-oPPCtFN2DPuM//c48SXb4TrFRjJtccg0YPXcAo0Lxq0=";
        };
      }
      {
        name = "plugin-foreign-env";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-foreign-env";
          rev = "7f0cf09";
          hash = "sha256-4+k5rSoxkTtYFh/lEjhRkVYa2S4KEzJ/IJbyJl+rJjQ=";
        };
      }
      {
        name = "theme-solarfish";
        src = pkgs.fetchFromGitHub {
          owner = "thesilican";
          repo = "theme-solarfish";
          rev = "48d4f4c";
          hash = "sha256-Zksi3+l/464il3IB3Rs05KSID6rhrglsZfNdfTaYYnA=";
        };
      }
      {
        name = "plugin-direnv";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-direnv";
          rev = "0221a4d";
          hash = "sha256-50tMKwtXtJBpgZ42JfJKyIWgusu4xZ9/yCiGKDfqyhE=";
        };
      }
    ];
  };

  programs.git = {
    enable = true;

    userEmail = "sebastian@zawadzki.tech";
    userName = "Sebastian Zawadzki";

    extraConfig = { init.defaultBranch = "master"; };

    ignores = [ ".DS_Store" ];
  };

  home.file = {
    ".config/alacritty/alacritty.yml".source =
      ./dotfiles/alacritty/darwin-alacritty.yml;
    ".gnupg/gpg-agent.conf".source = ./dotfiles/gpg-agent.conf;
    ".gnupg/gpg.conf".source = ./dotfiles/gpg.conf;
    ".config/lulublock.txt".source = ./dotfiles/lulublock.txt;
    ".ssh/git".source = ./secrets/git;
    ".ssh/default".source = ./secrets/default;
    ".ssh/config".source = ./secrets/ssh_config;
  };

  # Linking dynamic files that might change on the destination.
  # While this doesn't guarantee immutability anymore, I am willing to make this sacrifice for these files.
  # Note that this list should be kept as small as possible, and expanded only if there is no other way.
  home.activation.copyFiles = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ln -sf ~/.config/nix/dotfiles/alacritty/theme.yml ~/.config/alacritty/theme.yml
    ln -sfF ~/.config/nix/emacs/ ~/.config/emacs
  '';

}
