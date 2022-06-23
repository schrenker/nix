{ config, pkgs, lib, ... }:
let unstable = import <unstable> { config = { allowUnfree = true; }; };
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
    git
    git-crypt
    jq
    kubectl
    neofetch
    nix-direnv
    nixfmt
    podman
    rnix-lsp
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
        src = builtins.fetchGit {
          url = "https://github.com/oh-my-fish/plugin-bang-bang";
          ref = "master";
        };
      }
      {
        name = "plugin-foreign-env";
        src = builtins.fetchGit {
          url = "https://github.com/oh-my-fish/plugin-foreign-env";
          ref = "master";
        };
      }
      {
        name = "theme-cmorrell.com";
        src = builtins.fetchGit {
          url = "https://github.com/oh-my-fish/theme-cmorrell.com";
          ref = "master";
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
    ".gnupg/gpg-agent.conf".source =
      config.lib.file.mkOutOfStoreSymlink ./dotfiles/gpg-agent.conf;
  };
}
