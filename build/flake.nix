{
  # this version contains essential graalvm fixes, but we will have to pin to a better tag once available
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/8c9fd3e564728e90829ee7dbac6edc972971cd0f";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    { self
    , nixpkgs
    , flake-utils
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
            name = "7mind/baboon-docker-detached";
            tag = "${baboon-bin.version}";

            config = {
              # here we may reference other packages  
              Env = [
                "BABOON_DIR=${baboon-bin}"
                # I don't know a way to make substitutions work globally, but we may just explicitly create correct PATH
                "PATH=${baboon-bin}/bin/baboon:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

              ];

              Entrypoint = [ "${baboon-bin}/bin/baboon" ];
            };

            copyToRoot = (with pkgs.dockerTools; [
              # no magic, see https://github.com/NixOS/nixpkgs/blob/8e177b1a5d868d67e086d6fc137ffcc1ad51db04/pkgs/build-support/docker/default.nix#L957
              binSh
              pkgs.coreutils

              # an alternative to binSh + pkgs.coreUtils              
              # pkgs.busybox

              usrBinEnv
              caCertificates
              fakeNss

              # won't work, there is no way to make bash unconditionally source a file  
              #              (pkgs.runCommand "etc-profile" { } ''
              #                   mkdir -p $out/etc
              #                   echo "export PATH=${baboon-bin}/bin/baboon:\$PATH" > $out/etc/profile
              #                   chmod 555 $out/etc/profile
              #                 '')

            ]);
            
            # may be useful for extended setup (also see other helpers) but not needed for just binary utilities
            #             ++ (with pkgs; [
            #              (buildEnv {
            #                name = "baboon-container-env";
            #                paths = [

            #                ];
            #                pathsToLink = [ "/bin" "/etc" "/var" ];
            #              })
            #            ]);

            created = "now";
          };

          baboon-bin = pkgs.stdenvNoCC.mkDerivation rec {
            pname = "baboon";
            version = "0.0.0-test";

            src = ./.;

            builder = pkgs.writeText "builder.sh" ''
              export PATH=${pkgs.coreutils}/bin:$PATH
              set -xe
              mkdir -p $out/bin
              cp $src/baboon $out/bin/baboon
              chmod +x $out/bin/baboon
            '';

            meta = with pkgs.lib; {
              description = "Baboon standalone build demo";
              homepage = "https://github.com/7mind/baboon";
              license = [ licenses.mit ];
              maintainers = with maintainers; [ ];
              platforms = platforms.all;
            };
          };
        };
      }
    );
}
