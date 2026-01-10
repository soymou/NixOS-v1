{ pkgs, epkgs }:

epkgs.trivialBuild {
  pname = "ob-lean4";
  version = "20260110"; # Using today's date or just a version
  
  src = pkgs.fetchFromGitHub {
    owner = "soymou";
    repo = "ob-lean4";
    rev = "7e182baa6091e794a00940c96b919f59a6faa968";
    sha256 = "1ywjxi5iq8i0rwwfqi5mks9mjxi35v2lv0kr7v5asrj4ph8ybcfh";
  };

  packageRequires = with epkgs; [
    (import ./lean4-mode.nix { inherit pkgs epkgs; })
  ];
}
