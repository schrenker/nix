{ inputs, lib, pkgs, vars, ... }: {
  home.stateVersion = "23.11";
  home.username = "${vars.username}";
  home.homeDirectory = "${vars.homePrefix}/${vars.username}";

  programs.home-manager.enable = true;

  nixpkgs.config.allowUnfree = true;

  home.packages = with pkgs;
    [
      any-nix-shell
      arping
      cmake
      d2
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
      kubernetes-helm
      libgccjit
      nix-direnv
      nixfmt
      nodejs
      ripgrep
      tmux
      wget
    ] ++ [ inputs.nil.packages."${system}".nil ]
    ++ lib.optionals stdenv.isDarwin [ pkgs.pinentry_mac ]
    ++ lib.optionals stdenv.isLinux [ pkgs.emacs29-pgtk pkgs.jetbrains-mono ];

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      ${pkgs.any-nix-shell}/bin/any-nix-shell fish --info-right | source
    '';

    shellInit = builtins.readFile ./dotfiles/config.fish;

    shellAliases = {
      wget = "wget --hsts-file ~/.config/wget/wget-hsts";
      nq = "networkQuality";
      k = "kubectl";
      kx = "kubectx";
      kns = "kubens";
      drs = "darwin-rebuild switch --flake ${vars.switchPath}";
      nrs = "nixos-rebuild switch --flake ${vars.switchPath}";
      hrs = "home-manager switch --flake ${vars.switchPath}";
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
    ".ssh/git".source = ./secrets/${vars.secretDir}/git;
    ".ssh/default".source = ./secrets/${vars.secretDir}/default;
    ".ssh/config".source = ./secrets/${vars.secretDir}/ssh_config;
  };

  # Linking dynamic files that might change on the destination.
  # While this doesn't guarantee immutability anymore, I am willing to make this sacrifice for these files.
  # Note that this list should be kept as small as possible, and expanded only if there is no other way.
  home.activation.copyFiles = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    ln -sf ~/.config/nix/dotfiles/alacritty/theme.yml ~/.config/alacritty/theme.yml
    ln -sfFn ~/.config/nix/emacs ~/.config/emacs
  '';

}
