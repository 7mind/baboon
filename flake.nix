{
  description = "baboon build environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/24.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.buildPackages; [
            ncurses
            #graalvm-ce

            (graalvm-ce.overrideDerivation (oldAttrs: {

              postInstall =
                let
                  darwinArgs = pkgs.lib.optionals stdenv.hostPlatform.isDarwin [
                    "-ENIX_BINTOOLS"
                    "-ENIX_CC"
                    "-ENIX_CFLAGS_COMPILE"
                    "-ENIX_LDFLAGS"
#                    "-EbuildInputs"
                    "-EcmakeFlags"
                    "-EnativeBuildInputs"
                    "-EpropagatedBuildInputs"
                    "-EpropagatedNativeBuildInputs"
                    "-ENIX_CC_WRAPPER_TARGET_HOST_${pkgs.stdenv.cc.suffixSalt}"
                    "-ENIX_BINTOOLS_WRAPPER_TARGET_HOST_${pkgs.stdenv.cc.suffixSalt}"
                  ];

                  darwinFlags = (map (f: "--add-flags '${f}'") darwinArgs);
                in

                pkgs.lib.replaceStrings [ "/bin/native-image" ] [
                  "/bin/native-image ${toString (darwinFlags)}"
                ] oldAttrs.postInstall;
            }))

            sbt
            dotnet-sdk_7
          ];
        };
      }
    );
}
