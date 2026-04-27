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
        # GraalVM only supports --libc=musl on x86_64-linux (not aarch64).
        # Only referenced in isX86Linux guards; Nix laziness prevents evaluation elsewhere.
        isX86Linux = system == "x86_64-linux";
        arch = pkgs.stdenv.hostPlatform.parsed.cpu.name;
        muslCc = pkgs.pkgsMusl.stdenv.cc;
        muslZlibStatic = pkgs.pkgsMusl.zlib.static;
        graalvm = pkgs.graalvmPackages.graalvm-ce;

        muslGccAlias = pkgs.runCommand "musl-gcc-alias" {} ''
          mkdir -p $out/bin
          ln -s ${muslCc}/bin/cc $out/bin/${arch}-linux-musl-gcc
        '';

        nativeImageMusl = pkgs.writeShellScriptBin "native-image" ''
          export PATH="${muslGccAlias}/bin:${muslCc}/bin:$PATH"
          exec "${graalvm}/lib/svm/bin/native-image" \
            -H:CLibraryPath=${pkgs.musl}/lib \
            -H:CLibraryPath=${muslZlibStatic}/lib \
            "$@"
        '';

        # ---------------------------------------------------------------
        # Swift toolchain (Linux only)
        # ---------------------------------------------------------------
        # nixpkgs `pkgs.swift` on Linux compiles but `swift test` fails: it
        # crashes on a missing `libIndexStore.so` (genuine nixpkgs gap, not a
        # wiring issue) and the swiftpm manifest binaries don't have rpaths
        # propagated to libdispatch. Apple's official Linux tarball bundles
        # all corelibs consistently, so we vendor it via FOD + autoPatchelf.
        #
        # The bundled clang then needs an FHS-style filesystem to find CRT
        # objects (Scrt1.o, crti.o, libgcc_s) at link time — nixpkgs doesn't
        # populate /usr/lib. We wrap each Swift entrypoint in `buildFHSEnv`,
        # which is the canonical nix idiom for "Apple/Google prebuilt
        # toolchain on NixOS" (same pattern as `vscode-fhs`, `appimage-run`,
        # JetBrains IDEs).
        #
        # Trade-off: ~700 MiB tarball (cached after first build), but
        # reproducible (FOD-hashed) and matches the Swift CI runs against.
        appleSwiftToolchain =
          let
            swiftVersion = "5.10.1";
            ubuntuVersion = "22.04";
            ubuntuTag = builtins.replaceStrings ["."] [""] ubuntuVersion;
            isAarch64 = pkgs.stdenv.hostPlatform.isAarch64;
            archSuffix = if isAarch64 then "-aarch64" else "";
            urlSuffix  = if isAarch64 then "-aarch64" else "";
            tarballHash =
              if isAarch64 then
                "sha256-hxsA8Kf5bg0o2lOyMhgckAp1QMtL43/kkWwVq0Efg8k="
              else
                "sha256-yrG//9M7eevUn0t0db72x+stYM85SMvGk9Ya+r0jwoI=";
          in
          pkgs.stdenv.mkDerivation {
            pname = "swift-apple";
            version = swiftVersion;
            src = pkgs.fetchurl {
              url = "https://download.swift.org/swift-${swiftVersion}-release/ubuntu${ubuntuTag}${urlSuffix}/swift-${swiftVersion}-RELEASE/swift-${swiftVersion}-RELEASE-ubuntu${ubuntuVersion}${archSuffix}.tar.gz";
              hash = tarballHash;
            };
            nativeBuildInputs = [ pkgs.autoPatchelfHook ];
            buildInputs = with pkgs; [
              stdenv.cc.cc.lib
              zlib
              ncurses5
              libxml2_13         # libxml2.so.2 — needed by swift-test, FoundationXML, lldb
              sqlite
              python3
              libuuid
              icu
              libedit
              curl
              libxcrypt-legacy   # libcrypt.so.1
            ];
            # lldb-only deps that nixpkgs doesn't ship at the expected SONAME.
            # Swift build/test work fine; only debugging would hit them.
            autoPatchelfIgnoreMissingDeps = [
              "libpython3.10.so.1.0"
              "libedit.so.2"
            ];
            installPhase = ''
              runHook preInstall
              mkdir -p $out
              cp -R usr/* $out/
              runHook postInstall
            '';
            dontStrip = true;
          };

        # FHS-wrapped Swift entrypoints. Inside the FHS env, Apple's bundled
        # clang sees /usr/lib/x86_64-linux-gnu/Scrt1.o, libgcc_s, etc. exactly
        # where it expects them. Each entrypoint is a thin wrapper that enters
        # the FHS env and re-execs the real binary.
        #
        # Why one wrapper per entrypoint: swiftpm spawns subprocesses (swiftc,
        # swift-frontend, clang, lld) by absolute path against the toolchain's
        # bin dir. We can't redirect those mid-run, so we run the *parent*
        # (`swift` / `swift-build` / `swift-test`) inside FHS and let it spawn
        # whatever it wants — the children inherit the FHS namespace.
        mkSwiftFhs = entrypoint: pkgs.buildFHSEnv {
          name = entrypoint;
          targetPkgs = fhsPkgs: with fhsPkgs; [
            appleSwiftToolchain
            # CRT + linker bits clang/ld.gold look for under /usr/lib.
            glibc glibc.dev
            gcc-unwrapped gcc-unwrapped.lib
            binutils
            # Runtime deps the swift test binary loads at startup.
            zlib ncurses5 libxml2_13 sqlite libuuid icu libedit curl
            libxcrypt-legacy
            python3
            # IANA timezone database — Foundation's `TimeZone(identifier:)` looks
            # up zone files under /usr/share/zoneinfo. Without this, fixtures
            # that build random dates crash on `TimeZone(identifier: "UTC")!`
            # returning nil.
            tzdata
          ];
          runScript = entrypoint;
          # Pass through XDG / HOME / build-output dirs so swiftpm caches +
          # outputs end up where the user expects, not in the FHS overlay.
          extraBwrapArgs = [];
          unsharePid = false;
          unshareUser = false;
          unshareIpc = false;
          unshareNet = false;
          unshareUts = false;
          unshareCgroup = false;
        };

        appleSwift = pkgs.symlinkJoin {
          name = "swift-fhs-suite";
          paths = builtins.map mkSwiftFhs [
            "swift"
            "swiftc"
            "swift-build"
            "swift-test"
            "swift-package"
            "swift-run"
            "sourcekit-lsp"
          ];
          meta = {
            description = "Apple Swift 5.10.1 toolchain (FHS-wrapped, Linux)";
            longDescription = ''
              Apple's official Swift toolchain (downloaded from swift.org) wrapped in
              `pkgs.buildFHSEnv` so the bundled clang, swiftpm, lldb, etc. find their
              CRT/runtime dependencies on NixOS where /usr/lib doesn't exist.
              Provides `swift`, `swiftc`, `swift-build`, `swift-test`, `swift-package`,
              `swift-run`, `sourcekit-lsp`. Drop-in replacement for `pkgs.swift` on Linux
              including full `swift test` + XCTest support.
            '';
            mainProgram = "swift";
            platforms = pkgs.lib.platforms.linux;
            license = pkgs.lib.licenses.asl20;
          };
        };
      in
      {
        packages = rec {
          baboon = pkgs.stdenv.mkDerivation {
            inherit version;
            pname = "baboon";
            src = ./.;
            nativeBuildInputs = sbtSetup.nativeBuildInputs ++ [ pkgs.curl ]
              ++ pkgs.lib.optionals isX86Linux [
                nativeImageMusl
              ];
            inherit (sbtSetup) JAVA_HOME;

            buildPhase = ''
              ${sbtSetup.setupScript}
              ${pkgs.lib.optionalString isX86Linux ''
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

          # JVM-based distribution: a collection of jars plus a launcher script,
          # produced by sbt-native-packager's Universal/stage. Builds much faster
          # than the native-image binary and is useful when GraalVM is not
          # available or not desired.
          baboon-jvm = pkgs.stdenv.mkDerivation {
            inherit version;
            pname = "baboon-jvm";
            src = ./.;
            nativeBuildInputs = sbtSetup.nativeBuildInputs ++ [ pkgs.makeWrapper ];
            inherit (sbtSetup) JAVA_HOME;

            buildPhase = ''
              ${sbtSetup.setupScript}
              ${pkgs.lib.optionalString pkgs.stdenv.isDarwin ''
                HOME="$TMPDIR" \
                SBT_OPTS="-Duser.home=$TMPDIR -Dsbt.global.base=$TMPDIR/.sbt -Dsbt.ivy.home=$TMPDIR/.ivy2 -Divy.home=$TMPDIR/.ivy2 -Dsbt.boot.directory=$TMPDIR/.sbt/boot" \
                sbt baboonJVM/Universal/stage
              ''}
              ${pkgs.lib.optionalString (!pkgs.stdenv.isDarwin) ''
                sbt baboonJVM/Universal/stage
              ''}
            '';

            installPhase = ''
              mkdir -p $out/share $out/bin
              # The launcher script resolves lib_dir as ../lib relative to its
              # own location, so the staged bin/ + lib/ layout must be preserved.
              cp -r baboon-compiler/.jvm/target/universal/stage $out/share/baboon-jvm
              chmod +x $out/share/baboon-jvm/bin/baboon
              makeWrapper $out/share/baboon-jvm/bin/baboon $out/bin/baboon-jvm \
                --set JAVA_HOME ${pkgs.jdk.home} \
                --prefix PATH : ${pkgs.jdk}/bin
            '';
          };

          default = baboon;
        } // pkgs.lib.optionalAttrs pkgs.stdenv.isLinux {
          # ---------------------------------------------------------------
          # Swift toolchain outputs (Linux-only, x86_64 + aarch64)
          # ---------------------------------------------------------------
          # Public flake API for downstream consumers that need a working
          # `swift test` on NixOS. Use as:
          #
          #   inputs.baboon.url = "github:7mind/baboon";
          #   environment.systemPackages = [ inputs.baboon.packages.${system}.swift ];
          #
          # Or in a devShell:
          #
          #   buildInputs = [ inputs.baboon.packages.${system}.swift ];
          #
          # `swift` is the combined suite (all Swift entrypoints under bin/).
          # `swift-toolchain-unwrapped` is the raw Apple tarball after autoPatchelf,
          # exposed for advanced users who want to wrap it themselves.
          swift = appleSwift;
          swift-toolchain-unwrapped = appleSwiftToolchain;
        };

        devShells.default = pkgs.mkShell ({
          nativeBuildInputs =
            pkgs.lib.optionals isX86Linux [
              nativeImageMusl
            ] ++
            (with pkgs.buildPackages; [
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
              tree-sitter

              kotlin
              gradle

              maven

              dart
            ]) ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
              # Apple Swift toolchain (FOD + buildFHSEnv) — see `appleSwiftToolchain`
              # and `mkSwiftFhs` above. Replaces nixpkgs `swift` + `swiftpm` because
              # those can't run `swift test` on Linux due to missing libIndexStore.so
              # and broken libdispatch rpaths.
              appleSwift
            ];
        });
      }
    );
}
