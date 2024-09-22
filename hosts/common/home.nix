{ inputs, lib, pkgs, ... }: {
  home.stateVersion = "23.11";
  home.username = "sebastian";

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs; [
    # NIX
    any-nix-shell
    nix-direnv
    nixfmt

    # CONTAINERS
    kind
    kubectl
    kubectx
    kubernetes-helm

    # GIT
    git
    git-crypt
    gnupg
    gnutls

    # UTILS
    arping
    (aspellWithDicts (dicts: with dicts; [ en en-computers pl ]))
    fd
    fish
    jq
    poppler_utils
    proselint
    yq-go
    wget

    # DEPS
    cmake
    libgccjit
    nodejs
    ripgrep
  ];

  fonts.fontconfig.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source
    '';

    shellInit = builtins.readFile ./dotfiles/fish/config.fish;

    shellAliases = {
      wget = "wget --hsts-file ~/.config/wget/wget-hsts";
      nq = "networkQuality";
      k = "kubectl";
      kg = "kubectl get";
      kgp = "kubectl get pods";
      kdp = "kubectl describe pod";
      kd = "kubectl describe";
      kx = "kubectx";
      kns = "kubens";
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
        src = inputs.fish-plugin-bang-bang;
      }
      {
        name = "plugin-foreign-env";
        src = inputs.fish-plugin-foreign-env;
      }
      {
        name = "theme-solarfish";
        src = inputs.fish-plugin-theme-solarfish;
      }
      {
        name = "z";
        src = inputs.fish-plugin-z;
      }
      {
        name = "plugin-direnv";
        src = inputs.fish-plugin-direnv;
      }
      {
        name = "emacs-eat-integration";
        src = ./dotfiles/fish/eat-integration;
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
    ".gnupg/gpg-agent.conf".source = ./dotfiles/gpg-agent.conf;
    ".gnupg/gpg.conf".source = ./dotfiles/gpg.conf;
    ".npmrc".source = ./dotfiles/.npmrc;
  };

  # Linking dynamic files that might change on the destination.
  # While this doesn't guarantee immutability anymore, I am willing to make this sacrifice for these files.
  # Note that this list should be kept as small as possible, and expanded only if there is no other way.
  home.activation.copyFiles = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ln -sfFn ~/.config/nix/hosts/common/dotfiles/emacs ~/.config/emacs
  '';

}
