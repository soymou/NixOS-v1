{
  trivialBuild,
  fetchFromGitHub,
}:
trivialBuild rec {
  pname = "ob-lean4";
  version = "main-11-10-2025";
  src = fetchFromGitHub {
    owner = "Maverobot";
    repo = "ob-lean4";
    rev = "e2216aa61fd54b2abe3092247a5b08225db9b807";
    hash = "sha256-TYlhS32+YbSz8aRND2mAjgiOO5CBAHFqde0WUtDlfZo=";
  };
  packageRequires = [ ];
}
