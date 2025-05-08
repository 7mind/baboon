{
  description = "baboon build environment";

  # this version contains essential graalvm fixes, but we will have to pin to a better tag once available
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/8c9fd3e564728e90829ee7dbac6edc972971cd0f";

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
          baboon-container = pkgs.dockerTools.buildImage {
            # see https://ryantm.github.io/nixpkgs/builders/images/dockertools/
            name = "7mind/baboon-docker";
            tag = "${baboon.version}";

            config = {
              Entrypoint = [ "${baboon}/bin/baboon" ];
            };

            created = "now";
          };

          baboon = sbt.lib.mkSbtDerivation {
            pname = "baboon";

            version = "0.0.105";
            depsSha256 = "sha256-vgBlprxDQDq5Qo7wgDnqNzeh3Nj9qGdgCq77/b+e6vI=";

            pkgs = pkgs;
            src = ./.;


            nativeBuildInputs = with pkgs; [
              graalvm-ce
              curl
            ];

            # sbt update doesn't download everything, sbt compile seems to be the only safe option
            depsWarmupCommand = ''
              sbt compile
            '';

            buildPhase = ''
              sbt GraalVMNativeImage/packageBin
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
            graalvm-ce
            coursier

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
