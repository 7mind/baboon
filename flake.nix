{
  description = "baboon build environment";

  # this version contains essential graalvm fixes, but we will have to pin to a better tag once available
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/25.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  inputs.squish-find-the-brains.url = "github:7mind/squish-find-the-brains";
  inputs.squish-find-the-brains.inputs.nixpkgs.follows = "nixpkgs";
  inputs.squish-find-the-brains.inputs.flake-utils.follows = "flake-utils";

  inputs.mudyla.url = "github:7mind/mudyla";
  inputs.mudyla.inputs.nixpkgs.follows = "nixpkgs";

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , squish-find-the-brains
    , mudyla
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        coursierCache = squish-find-the-brains.lib.mkCoursierCache {
          inherit pkgs;
          lockfilePath = ./deps.lock.json;
        };

        sbtSetup = squish-find-the-brains.lib.mkSbtSetup {
          inherit pkgs coursierCache;
          jdk = pkgs.graalvmPackages.graalvm-ce;
        };
      in
      {
        packages = rec {
          baboon = pkgs.stdenv.mkDerivation {
            version = "0.0.141";
            pname = "baboon";
            src = ./.;
            nativeBuildInputs = sbtSetup.nativeBuildInputs ++ [ pkgs.curl ];
            inherit (sbtSetup) JAVA_HOME;

            buildPhase = ''
              ${sbtSetup.setupScript}
              sbt baboonJVM/GraalVMNativeImage/packageBin
            '';

            installPhase = ''
              mkdir -p $out/bin
              cp baboon-compiler/.jvm/target/graalvm-native-image/baboon $out/bin/baboon
            '';
          };
          default = baboon;
        };

        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs.buildPackages; [
            ncurses
            gitMinimal
            nix
            graalvmPackages.graalvm-ce
            coursier

            pkgs.sbt
            dotnet-sdk_9

            coreutils
            shellspec
            zip

            rsync

            squish-find-the-brains.packages.${system}.generate-lockfile
            mudyla.packages.${system}.default
            nodejs_24
          ];
        };
      }
    );
}
