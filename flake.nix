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

    zen-browser = {
      url = "github:0xc000022070/zen-browser-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

  };

  # Flake outputs
  outputs = { self, nixpkgs, home-manager, zen-browser, ... }@inputs:
  let 
    vars = import ./hosts/variables.nix;
  in {
    nixosConfigurations.${vars.username} = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit vars; };
      modules = [
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
