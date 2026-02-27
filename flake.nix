{
  description = "baboon build environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
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

        # GraalVM --libc=musl needs a musl cross-compiler and musl CLibraryPaths.
        # The Nix native-image wrapper injects glibc CLibraryPaths which conflict
        # with musl, so we create a custom wrapper that bypasses the Nix glibc
        # injection and provides musl paths instead.
        # Only referenced in isLinux guards; Nix laziness prevents evaluation on Darwin.
        arch = pkgs.stdenv.hostPlatform.parsed.cpu.name;
        muslCc = pkgs.pkgsCross.musl64.stdenv.cc;
        muslZlibStatic = pkgs.pkgsMusl.zlib.static;
        graalvm = pkgs.graalvmPackages.graalvm-ce;

        muslGccAlias = pkgs.runCommand "musl-gcc-alias" {} ''
          mkdir -p $out/bin
          ln -s ${muslCc}/bin/${arch}-unknown-linux-musl-gcc $out/bin/${arch}-linux-musl-gcc
        '';

        nativeImageMusl = pkgs.writeShellScriptBin "native-image" ''
          export PATH="${muslGccAlias}/bin:${muslCc}/bin:$PATH"
          exec "${graalvm}/lib/svm/bin/native-image" \
            -H:CLibraryPath=${pkgs.musl}/lib \
            -H:CLibraryPath=${muslZlibStatic}/lib \
            "$@"
        '';
      in
      {
        packages = rec {
          baboon = pkgs.stdenv.mkDerivation {
            inherit version;
            pname = "baboon";
            src = ./.;
            nativeBuildInputs = sbtSetup.nativeBuildInputs ++ [ pkgs.curl ]
              ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
                nativeImageMusl
              ];
            inherit (sbtSetup) JAVA_HOME;

            buildPhase = ''
              ${sbtSetup.setupScript}
              ${pkgs.lib.optionalString pkgs.stdenv.isLinux ''
                export PATH="${nativeImageMusl}/bin:$PATH"
              ''}
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

            kotlin
            gradle

            maven

            dart
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            swift
            swiftpm
          ];

          # Prepend our musl-aware native-image wrapper before the Nix GraalVM one
          shellHook = pkgs.lib.optionalString pkgs.stdenv.isLinux ''
            export PATH="${nativeImageMusl}/bin:$PATH"
          '';
        };
      }
    );
}
