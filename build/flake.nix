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
              Entrypoint = [ "${baboon-bin}/bin/baboon" ];
            };

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
