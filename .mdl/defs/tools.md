# Baboon Tools

This file defines workflow-related actions for the Baboon project.

# action: fmt

Format Scala code using scalafmt.

```bash
cs launch scalafmt -- --non-interactive || true
git add . || true
ret success:bool=true
```

# action: flake-refresh

Refresh Nix flake configuration.

```bash
function do_update() {
  sed -i -E \
      -e "s/version\s*=\s*\"([a-z0-9-]\.?)+\";/version = \"$1\";/g" \
      $2
}


PKG_VERSION=$(cat version.sbt | sed -r 's/.*\"(.*)\".**/\1/' | sed -E "s/-SNAPSHOT//")

nix flake update

do_update "$PKG_VERSION" ./flake.nix

sbt-nix-lockfile lockfile-config.json > deps.lock.json

git add . || true

nix flake check

ret success:bool=true
```

