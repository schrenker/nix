{ config, pkgs, lib, ... }:
let
  unstable = import <unstable> { config = { allowUnfree = true; }; };
in
{
  home.stateVersion = "22.05";
  home.username = "ansible";
  home.homeDirectory = "/Users/ansible";

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    arping
    aspell
    coreutils-prefixed
    fish
    nix-direnv
    git
    go
    golangci-lint
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

    shellInit = builtins.readFile ./fish/shellInit.fish;

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
          sha256 = "LnIPFnkfp0qtmbfKlupPTr+ThUwT9/TD+IAT8GBMnmk=";
        };
      }
      {
        name = "plugin-foreign-env";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-foreign-env";
          rev = "b3dd471bcc885b597c3922e4de836e06415e52dd";
          sha256 = "LnIPFnkfp0qtmbfKlupPTr+ThUwT9/TD+IAT8GBMnmk=";
        };
      }
      {
        name = "theme-cmorrell.com";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "theme-cmorrell.com";
          rev = "de213619ec87a3434781dd6ea8b47e219e46fb11";
          sha256 = "LnIPFnkfp0qtmbfKlupPTr+ThUwT9/TD+IAT8GBMnmk=";
        };
      }
    ];
  };

  programs.go = {
    enable = true;
    package = pkgs.go_1_18;
    goPath = ".local/go";
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

  programs.gpg = {
    enable = true;
  };
#   home.file = {
#     ".gnupg/pubkey.pub".source = config.lib.file.mkOutOfStoreSymlink ./home/gnupg/f.pub;
#     ".gnupg/gpg-agent.conf".text = ''
#       # https://github.com/drduh/config/blob/master/gpg-agent.conf
#       # https://www.gnupg.org/documentation/manuals/gnupg/Agent-Options.html
#       enable-ssh-support
#       ttyname $GPG_TTY
#       default-cache-ttl 60
#       max-cache-ttl 120
#       pinentry-program ${pkgs.pinentry_mac}/Applications/pinentry-mac.app/Contents/MacOS/pinentry-mac
#     '';
#     ".local/bin/dir_select".source = config.lib.file.mkOutOfStoreSymlink ./home/zsh/dir_select;
#     ".local/bin/update-tf.sh".source = config.lib.file.mkOutOfStoreSymlink ./home/zsh/update-tf.sh;

#     # secrets
#     ".aws/config".source = config.lib.file.mkOutOfStoreSymlink ./secrets/aws/config;
#     ".aws/credentials".source = config.lib.file.mkOutOfStoreSymlink ./secrets/aws/credentials;
#     ".ssh/cloud".source = config.lib.file.mkOutOfStoreSymlink ./secrets/ssh/cloud;
#     ".ssh/config".source = config.lib.file.mkOutOfStoreSymlink ./secrets/ssh/config;
#     ".netrc".source = config.lib.file.mkOutOfStoreSymlink ./secrets/netrc;
#   };
}
