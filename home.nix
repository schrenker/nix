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
    cmake
    fd
    fish
    git
    git-crypt
    gnupg
    kind
    kubectl
    kubectx
    jq
    nix-direnv
    nodejs
    pinentry_mac
    powershell
    ripgrep
    tmux
    wget
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.fish = {
    enable = true;

    #    shellInit = builtins.readFile ./dotfiles/config.fish;
    shellInit = ''
function __nixos_path_fix -d "fix PATH value"
set -l result (string replace '$HOME' "$HOME" $__nixos_path_original)
for elt in $PATH
  if not contains -- $elt $result
    set -a result $elt
  end
end
set -g PATH $result
end

    __nixos_path_fix
    '';


    shellAliases = {
      wget = "wget --hsts-file ~/.config/wget/wget-hsts";
      vi = "/opt/homebrew/bin/emacsclient -c -a emacs $argv &>/dev/null";
      nQ = "networkQuality";
      fixproj = "rm ~/.config/emacs/.local/cache/treemacs-persist";
      docker = "podman";
      k = "kubectl";
      kx = "kubectx";
      kns = "kubens";
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
      {
        name = "plugin-direnv";
        src = builtins.fetchGit {
          url = "https://github.com/oh-my-fish/plugin-direnv";
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
    ".gnupg/gpg.conf".source =
      config.lib.file.mkOutOfStoreSymlink ./dotfiles/gpg.conf;
    ".config/lulublock.txt".source =
      config.lib.file.mkOutOfStoreSymlink ./dotfiles/lulublock.txt;
    ".config/iterm2/com.googlecode.iterm2.plist".source =
      config.lib.file.mkOutOfStoreSymlink
        ./dotfiles/iterm2/com.googlecode.iterm2.plist;

    "Library/Application Support/iTerm2/Scripts/AutoLaunch/auto_dark_mode.py".source =
      config.lib.file.mkOutOfStoreSymlink ./dotfiles/iterm2/auto_dark_mode.py;

    ".ssh/git".source = config.lib.file.mkOutOfStoreSymlink ./secrets/git;
    ".ssh/default".source =
      config.lib.file.mkOutOfStoreSymlink ./secrets/default;
    ".ssh/config".source =
      config.lib.file.mkOutOfStoreSymlink ./secrets/ssh_config;
  };

}
