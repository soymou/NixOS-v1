{
  trivialBuild,
  fetchFromGitHub,
  magit-section,
  dash,
  lsp-mode,
}:
trivialBuild rec {
  pname = "lean4-mode";
  version = "main-11-10-2025";
  src = fetchFromGitHub {
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
  };
  packageRequires = [
    magit-section
    dash
    lsp-mode
  ];
}
