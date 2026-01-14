{ config, lib, pkgs, ... }:

let
  kernel = config.boot.kernelPackages.kernel;
  aic8800 = config.boot.kernelPackages.stdenv.mkDerivation rec {
    pname = "aic8800-linux-driver";
    version = "unstable-2026-01-13";

    src = pkgs.fetchFromGitHub {
      owner = "shenmintao";
      repo = "aic8800d80";
      rev = "3a0945bdcc94dd15402b8e133f5cf53b9ace7ed6";
      sha256 = "0qrqj2c7xgwpg1024p7rczv7m35y6c0qrdf287vz0zyqnqy5ca7a";
    };

    hardeningDisable = [ "pic" "format" ];
    nativeBuildInputs = kernel.moduleBuildDependencies;

    # Point to the directory containing the Makefile
    preBuild = ''
      cd drivers/aic8800
    '';

    makeFlags = [
      "KDIR=${kernel.dev}/lib/modules/${kernel.modDirVersion}/build"
      "ARCH=${pkgs.stdenv.hostPlatform.linuxArch}"
    ];

    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib/modules/${kernel.modDirVersion}/kernel/drivers/net/wireless/aic8800
      # Copy modules - names might vary, using wildcard or best guess
      find . -name "*.ko" -exec cp {} $out/lib/modules/${kernel.modDirVersion}/kernel/drivers/net/wireless/aic8800/ \;
      runHook postInstall
    '';

    meta = with lib; {
      description = "Aicsemi aic8800 Wi-Fi driver (shenmintao fork)";
      homepage = "https://github.com/shenmintao/aic8800d80";
      license = licenses.mit;
      platforms = platforms.linux;
    };
  };
in
{
  boot.extraModulePackages = [ aic8800 ];
  # We'll load the modules if they exist. Common names:
  boot.kernelModules = [ "aic_load_fw" "aic8800_fdrv" ];
}
