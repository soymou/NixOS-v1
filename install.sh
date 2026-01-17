#!/usr/bin/env bash

echo "Introduce tu nombre de usuario:"
read username 

if [ -d "hosts/$username" ]
then
  echo "El directorio de hosts para $username ya existe."
else
  echo "Creando directorio de hosts para $username..."
  mkdir -p "hosts/$username"
  cp hosts/default/configuration.nix "hosts/$username/configuration.nix"
  cp hosts/default/home.nix "hosts/$username/home.nix"
  cp /etc/nixos/hardware-configuration.nix "hosts/$username/hardware-configuration.nix"
  nix-shell -p git --run "git add ."
fi


