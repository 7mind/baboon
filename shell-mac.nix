{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/24.05.tar.gz") { } }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs.buildPackages; [ ncurses sbt dotnet-sdk_7 ];
}
