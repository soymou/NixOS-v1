{ config, lib, pkgs, ... }:

let
  aic8800 = config.boot.kernelPackages.stdenv.mkDerivation rec {
    pname = "aic8800-linux-driver";
    version = "unstable-2025-01-13";

    src = pkgs.fetchFromGitHub {
      owner = "goecho";
      repo = "aic8800_linux_drvier";
      rev = "6b5680d3605f34bfa84618f880a04a240749acc3";
      sha256 = "0k405ik4z8m0f0r91j9da90cvs715dhz1i0zx78g0q52s5s3bs43";
    };

    hardeningDisable = [ "pic" "format" ];
    nativeBuildInputs = config.boot.kernelPackages.kernel.moduleBuildDependencies;

    makeFlags = [
      "KERNELDIR=${config.boot.kernelPackages.kernel.dev}/lib/modules/${config.boot.kernelPackages.kernel.modDirVersion}/build"
      "INSTALL_MOD_PATH=$(out)"
    ];

    meta = with lib; {
      description = "Aicsemi aic8800 Wi-Fi driver";
      homepage = "https://github.com/goecho/aic8800_linux_drvier";
      license = licenses.mit;
      platforms = platforms.linux;
    };
  };
in
{
  boot.extraModulePackages = [ aic8800 ];
  boot.kernelModules = [ "aic8800_fdrv" ];
}
