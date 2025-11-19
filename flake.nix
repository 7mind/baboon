{
  description = "baboon build environment";

  # this version contains essential graalvm fixes, but we will have to pin to a better tag once available
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/8c9fd3e564728e90829ee7dbac6edc972971cd0f";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.sbt-nix.url = "github:7mind/sbt-nix";
  inputs.sbt-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.sbt-nix.inputs.flake-utils.follows = "flake-utils";

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , sbt-nix
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        coursierCache = sbt-nix.lib.mkCoursierCache {
          inherit pkgs;
          lockfilePath = ./deps.lock.json;
        };

        sbtSetup = sbt-nix.lib.mkSbtSetup {
          inherit pkgs coursierCache;
          jdk = pkgs.graalvm-ce;
        };
      in
      {
        packages = rec {
          baboon = pkgs.stdenv.mkDerivation {
            version = "0.0.139";
            pname = "baboon";
            src = ./.;
            nativeBuildInputs = sbtSetup.nativeBuildInputs;
            inherit (sbtSetup) JAVA_HOME;

            buildPhase = ''
              ${sbtSetup.setupScript}
              ./run -v --nix :build
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp baboon-compiler/target/graalvm-native-image/baboon $out/bin/baboon
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
            dotnet-sdk_9

            coreutils
            shellspec
            nix
          ];
        };
      }
    );
}
