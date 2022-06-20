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
    nix-direnv
    git
    go
    golangci-lint
    jq
    kubectl
    mas
    neofetch
    podman
    shellcheck
    terraform
    tmux
    yamllint
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  # programs.go = {
  #   enable = true;
  #   package = unstable.go_1_18;
  #   goPrivate = [ "gitlab.shopware.com" ];
  #   goPath = "code/go";
  # };

  # programs.gpg = {
  #   enable = true;
  #   scdaemonSettings = {
  #     disable-ccid = true;
  #   };
  #   publicKeys = [{
  #     source = ./home/gnupg/f.pub;
  #     trust = "ultimate";
  #   }];
  # };

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

  # programs.zsh = {
  #   enable = true;
  #   enableCompletion = false;
  #   oh-my-zsh = {
  #     enable = true;
  #     theme = "trapd00r";
  #     plugins = ["git" "docker" "docker-compose" "aws"];
  #   };
  #   localVariables = {
  #     EDITOR = "nvim";
  #     PATH = "$PATH:$GOPATH/bin:$HOME/.local/bin";
  #   };
  #   sessionVariables = {
  #     DOCKER_BUILDKIT = 1;
  #   };
  #   shellAliases = {
  #     ykrestart = "gpgconf --reload scdaemon && gpgconf --kill gpg-agent && gpg-connect-agent updatestartuptty /bye";
  #     awsume = ". awsume";
  #     vi = "lvim";
  #     vim = "lvim";
  #     gpo = "git pull origin $(git_current_branch)";
  #   };
  #   initExtra = ''
  #     # yubikey setup
  #     export GIT_SSH="/usr/bin/ssh"
  #     export GPG_TTY="$(tty)"
  #     export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  #     gpgconf --launch gpg-agent
  #     # custom scripts
  #     ${builtins.readFile ./home/zsh/scripts.sh}
  #   '';
  # };

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
