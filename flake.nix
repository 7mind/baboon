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
          baboon = sbt.lib.mkSbtDerivation {
            pkgs = pkgs;
            version = "0.0.101";
            pname = "baboon";
            src = ./.;
            depsSha256 = "sha256-GemB9LkGFprzZ885VkiTrgVh9bpE1ttwf9g3qRsy6mQ=";
            nativeBuildInputs = with pkgs; [
              graalvm-ce
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
            graalvm-ce

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
