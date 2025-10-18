{
  description = "Your new nix config";

  inputs = {
    # Stable Nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Unstable Nixpkgs
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    hyprland.url = "github:hyprwm/Hyprland";

    nix-minecraft = {
      url = "github:Infinidoge/nix-minecraft";
    };

    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    systems.url = "github:nix-systems/default-linux";

    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixvim = {
      url = "github:nix-community/nixvim";
    };

    nvchad = {
      url = "github:soymou/nvchad";
      flake = false;
    };

    nix4nvchad = {
      url = "github:nix-community/nix4nvchad";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nvchad-starter.follows = "nvchad";
    }; 

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  
    illogical-flake = {
      url = "github:soymou/illogical-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = { self, nixpkgs, home-manager, illogical-flake, ... } @ inputs:
    let
      # Supported systems
      systems = [
        "aarch64-linux"
        "i686-linux"
        "x86_64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
      system = "x86_64-linux";

      # Load overlays
      allOverlays = import ./overlays { inherit inputs; };

      myOverlays = [
        allOverlays.additions
        allOverlays.modifications
        allOverlays.unstable-packages
      ] ++ allOverlays.nixpkgs.overlays;

      # Create a pkgs set with overlays
      mkPkgs = system: import nixpkgs {
        inherit system;
        overlays = myOverlays;
        config.allowUnfree = true;
      };
    in
    {
      # Custom packages accessible via `nix build`, `nix shell`, etc
      packages = forAllSystems (system:
        import ./pkgs { pkgs = mkPkgs system; }
      );

      # Formatter (used with `nix fmt`)
      formatter = forAllSystems (system: mkPkgs system).alejandra;

      templates = import ./dev-shells; 

      # Export overlays for use elsewhere
      overlays = allOverlays;

      # Export reusable modules
      nixosModules = (import ./modules).nixosModules;
      homeManagerModules = (import ./modules).homeManagerModules;

      # NixOS configuration
      nixosConfigurations = {
        mou = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit system inputs;
            outputs = self.outputs;
            inherit (illogical-flake.inputs) hyprland nur dotfiles;
          };
          modules = [
            ./nixos/configuration.nix
            inputs.nix-minecraft.nixosModules.minecraft-servers
            {
              nixpkgs.overlays = myOverlays ++ [ inputs.nix-minecraft.overlay ];
            }
            inputs.illogical-flake.nixosModules.default
          ];
        };
      };

      # Home Manager configuration
      homeConfigurations = {
        "mou@mou" = home-manager.lib.homeManagerConfiguration {
          pkgs = mkPkgs system;
          extraSpecialArgs = {
            inherit system inputs;
            outputs = self.outputs;
          };
          modules = [
            ./home-manager/home.nix
          ];
        };
      };
    };
}
