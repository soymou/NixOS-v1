{
  description = "A Nix-flake-based Python 2.7 development environment";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";

  outputs = inputs:
    let
      supportedSystems = [ "x86_64-linux" ];

      forEachSupportedSystem = f:
        inputs.nixpkgs.lib.genAttrs supportedSystems (system:
          f {
            system = system;
            pkgs = import inputs.nixpkgs {
              inherit system;
              config.allowUnsupportedSystem = true;
            };
          });

      version = "2.7";
    in
    {
      devShells = forEachSupportedSystem ({ pkgs, system }: 
        let
          python = pkgs.python2;
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs.python2Packages; [
              pip
              setuptools
              # Add more Python 2 packages here
            ];

            shellHook = ''
              echo "🐍 Entered Python ${python.version} devshell on ${system}"
              echo "⚠️  Python 2 is EOL — use with caution!"
            '';
          };
        });
    };
}

