{ pkgs, epkgs }:

epkgs.trivialBuild {
  pname = "ai-code-interface";
  version = "0.1.0";

  src = pkgs.fetchFromGitHub {
    owner = "tninja";
    repo = "ai-code-interface.el";
    rev = "1c17cf94a4f38a918a5ed61fc2958232c42a50fe";
    sha256 = "193dsir8c7lgssxhdjrbrf1w2czfz39kjqzizc4aqhf76dndj8sz";
  };

  packageRequires = with epkgs; [
    markdown-mode
    magit
  ];
}
