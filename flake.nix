{
  # Description
  description = "NixOS flake configuration";

  # Flake inputs 
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    home-manager = {
        url = "github:nix-community/home-manager";
        inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    illogical-dots = {
      url = "git+https://github.com/soymou/dots-hyprland/?submodules=1&rev=Laptop";
      flake = false;
    };
   
    illogical-flake = 
    {
      url = "github:soymou/illogical-flake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.dotfiles.follows = "illogical-dots";
    };

    burpsuitepro = {
      url = "github:soymou/Burpsuite-Professional";
      inputs.nixpkgs.follows = "nixpkgs";
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

    nix-minecraft.url = "github:Infinidoge/nix-minecraft";
  };

  # Flake outputs
  outputs = { self,
    nixpkgs,
    home-manager,
    zen-browser,
    illogical-flake,
    burpsuitepro,
    nix4nvchad,
    nix-minecraft,
    ... 
    }@inputs:
  let 
    vars = import ./hosts/variables.nix;
  in {
 	
    templates = import ./dev-shells;

    nixosConfigurations.${vars.username} = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit vars; inherit inputs; };
      modules = [
        {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlays.default
            inputs.nix-minecraft.overlay
          ];
        }
        ./hosts/${vars.username}/configuration.nix
        home-manager.nixosModules.home-manager
        {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users."${vars.username}" = import ./hosts/${vars.username}/home.nix;
            home-manager.extraSpecialArgs = {inherit vars; inherit inputs;};
        }
      ];
    };
  };
}
