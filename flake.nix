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

        # Read version from version.sbt
        versionSbt = builtins.readFile ./version.sbt;
        versionMatch = builtins.match ''.*"([0-9]+\.[0-9]+\.[0-9]+)(-SNAPSHOT)?".*'' versionSbt;
        version = builtins.elemAt versionMatch 0;

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
            inherit version;
            pname = "baboon";
            src = ./.;
            nativeBuildInputs = sbtSetup.nativeBuildInputs ++ [ pkgs.curl ];
            inherit (sbtSetup) JAVA_HOME;

            buildPhase = ''
              ${sbtSetup.setupScript}
              ${pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
                HOME="$TMPDIR" \
                SBT_OPTS="-Duser.home=$TMPDIR -Dsbt.global.base=$TMPDIR/.sbt -Dsbt.ivy.home=$TMPDIR/.ivy2 -Divy.home=$TMPDIR/.ivy2 -Dsbt.boot.directory=$TMPDIR/.sbt/boot" \
                sbt baboonJVM/GraalVMNativeImage/packageBin
              ''}
              ${pkgs.lib.optionalString (!pkgs.stdenv.isDarwin) ''
                sbt baboonJVM/GraalVMNativeImage/packageBin
              ''}
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

            rustc
            cargo

            squish-find-the-brains.packages.${system}.generate-lockfile
            mudyla.packages.${system}.default
            nodejs_24
          ];
        };
      }
    );
}
