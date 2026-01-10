{ pkgs, epkgs }:

epkgs.trivialBuild {
  pname = "lean4-mode";
  version = "1388f9d";

  src = pkgs.fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    sha256 = "06f4h8ihwp5yhbdpjqbp10rr1jz56s3vr43a6nq51iwklk55qwg9";
  };

  packageRequires = with epkgs; [
    dash
    f
    flycheck
    s
    lsp-mode
    magit-section
  ];
}
