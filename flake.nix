{
  description = "baboon build environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/24.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}; in
        {
          devShells.default = pkgs.mkShell {
                                nativeBuildInputs = with pkgs.buildPackages; [
                                  ncurses
                                  graalvm-ce
                                  sbt
                                  dotnet-sdk_7
                                ];

                                  shellHook = ''
                                    export NIX_CC_SUFFIX_SALT=${pkgs.stdenv.cc.suffixSalt}
                                  '';
                              };
        }
      );
}