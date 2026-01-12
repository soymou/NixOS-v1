{ pkgs, epkgs }:

epkgs.trivialBuild {
  pname = "ob-lean4";
  version = "20260110"; # Using today's date or just a version
  
  src = pkgs.fetchFromGitHub {
    owner = "soymou";
    repo = "ob-lean4";
    rev = "b6b7a3bbc80fa3cb9e92c975253e8f1049946d9b";
    sha256 = "sha256-ygOf0Fb7zlizFZp2IE4jKA+/Ev3fIBkIgeyVZ2Nx+v0=";
  };

  packageRequires = with epkgs; [
    (import ./lean4-mode.nix { inherit pkgs epkgs; })
  ];
}
