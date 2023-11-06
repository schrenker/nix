{
  description = "Darwin flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    fish-plugin-bang-bang.url = "github:oh-my-fish/plugin-bang-bang";
    fish-plugin-bang-bang.flake = false;
    fish-plugin-foreign-env.url = "github:oh-my-fish/plugin-foreign-env";
    fish-plugin-foreign-env.flake = false;
    fish-plugin-theme-solarfish.url = "github:thesilican/theme-solarfish";
    fish-plugin-theme-solarfish.flake = false;
    fish-plugin-direnv.url = "github:oh-my-fish/plugin-direnv";
    fish-plugin-direnv.flake = false;
  };

  outputs = { self, darwin, home-manager, nixpkgs, ... }@inputs:
    let
      darwinVars = { home = "Users"; };
      linuxVars = { home = "home"; };
    in {
      darwinConfigurations."Macbook" = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          ./darwin-configuration.nix
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sebastian = { imports = [ ./home.nix ]; };
            home-manager.extraSpecialArgs = { vars = darwinVars; inherit inputs; };
          }
        ];
      };

      nixosConfigurations."GFT" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.sebastian = import ./home.nix;
            home-manager.extraSpecialArgs = { vars = linuxVars; };
          }
        ];
      };
    };
}
