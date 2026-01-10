{ pkgs, epkgs }:

epkgs.trivialBuild {
  pname = "gemini-cli";
  version = "0.1.0";

  src = pkgs.fetchFromGitHub {
    owner = "linchen2chris";
    repo = "gemini-cli.el";
    rev = "c28aef428733abae03ca1367a10beda06f65cc68";
    sha256 = "1rwxx80swsk65awdqdnaiawsbs0d3vc55ykznkjr9q641anx0d18";
  };

  packageRequires = with epkgs; [
    markdown-mode
    request
  ];
}
