{
  description = "Darwin flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    flake-parts.url = "github:hercules-ci/flake-parts";

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    fish-plugin-bang-bang = {
      url = "github:oh-my-fish/plugin-bang-bang";
      flake = false;
    };

    fish-plugin-foreign-env = {
      url = "github:oh-my-fish/plugin-foreign-env";
      flake = false;
    };

    fish-plugin-theme-solarfish = {
      url = "github:schrenker/solarfish";
      flake = false;
    };

    fish-plugin-z = {
      url = "github:jethrokuan/z";
      flake = false;
    };

    fish-plugin-direnv = {
      url = "github:oh-my-fish/plugin-direnv";
      flake = false;
    };
  };

  outputs = inputs@{ self, flake-parts, darwin, home-manager, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      flake = {
        darwinConfigurations."Macbook" = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            ./hosts/macbook/darwin-configuration.nix
            home-manager.darwinModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = true;
              home-manager.users.sebastian = {
                imports = [ ./hosts/common/home.nix ./hosts/macbook/home.nix ];
              };
              home-manager.extraSpecialArgs = { inherit inputs; };
            }
          ];
        };

        homeConfigurations."WSL2" = home-manager.lib.homeManagerConfiguration {
          # system = "x86_64-linux";
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [
            ./hosts/common/home.nix
            ./hosts/wsl2/home.nix
            {
              home = {
                username = "sebastian";
                homeDirectory = "/home/sebastian";
                stateVersion = "23.11";
              };
            }
          ];
          extraSpecialArgs = { inherit inputs; };
        };
      };

      perSystem = { config, self', inputs', pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ inputs.nil.packages.${system}.nil ];
        };
      };
    };
}
