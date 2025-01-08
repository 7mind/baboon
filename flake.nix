{
  description = "baboon build environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/24.05";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.sbt.url = "github:zaninime/sbt-derivation";
  inputs.sbt.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , sbt
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = rec {
          baboon = sbt.lib.mkSbtDerivation {
            pkgs = pkgs;
            version = "0.0.90";
            pname = "baboon";
            src = ./.;
            depsSha256 = "sha256-aHR00f3o1CgW7ciqUmzF2AWhSRQvh2vz94EUTWQvTBA=";
            nativeBuildInputs = with pkgs; [
              # graalvm-ce

              # https://github.com/NixOS/nixpkgs/issues/350909
              (graalvm-ce.overrideDerivation (oldAttrs: {

                postInstall =
                  let
                    darwinArgs = pkgs.lib.optionals stdenv.hostPlatform.isDarwin [
                      "-ENIX_BINTOOLS"
                      "-ENIX_CC"
                      "-ENIX_CFLAGS_COMPILE"
                      "-ENIX_LDFLAGS"
                      "-ENIX_CC_WRAPPER_TARGET_HOST_${pkgs.stdenv.cc.suffixSalt}"
                      "-ENIX_BINTOOLS_WRAPPER_TARGET_HOST_${pkgs.stdenv.cc.suffixSalt}"
                    ];

                    darwinFlags = (map (f: "--add-flags '${f}'") darwinArgs);
                  in

                  pkgs.lib.replaceStrings [ "/bin/native-image" ] [
                    "/bin/native-image ${toString (darwinFlags)}"
                  ]
                    oldAttrs.postInstall;
              }))
            ];
            depsWarmupCommand = ''
              sbt update
            '';
            buildPhase = ''
              ./build.sh build
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp target/graalvm-native-image/baboon $out/bin/baboon
            '';
          };
          default = baboon;
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.buildPackages; [
            ncurses
            gitMinimal
            # graalvm-ce
            # https://github.com/NixOS/nixpkgs/issues/350909
            (graalvm-ce.overrideDerivation (oldAttrs: {

              postInstall =
                let
                  darwinArgs = pkgs.lib.optionals stdenv.hostPlatform.isDarwin [
                    "-ENIX_BINTOOLS"
                    "-ENIX_CC"
                    "-ENIX_CFLAGS_COMPILE"
                    "-ENIX_LDFLAGS"
                    "-ENIX_CC_WRAPPER_TARGET_HOST_${pkgs.stdenv.cc.suffixSalt}"
                    "-ENIX_BINTOOLS_WRAPPER_TARGET_HOST_${pkgs.stdenv.cc.suffixSalt}"
                  ];

                  darwinFlags = (map (f: "--add-flags '${f}'") darwinArgs);
                in

                pkgs.lib.replaceStrings [ "/bin/native-image" ] [
                  "/bin/native-image ${toString (darwinFlags)}"
                ]
                  oldAttrs.postInstall;
            }))

            pkgs.sbt
            dotnet-sdk_8

            coreutils
            shellspec
            nix
          ];
        };
      }
    );
}
