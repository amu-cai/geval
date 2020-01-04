# Run using:
#
#     $(nix-build --no-link -A fullBuildScript)
# also the following might be needed:
# sysctl kernel.unprivileged_userns_clone=1
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  cabalPackageName = "geval";
  compiler = "ghc844"; # matching stack.yaml

  # Pin static-haskell-nix version.
  static-haskell-nix =
    if builtins.pathExists ../.in-static-haskell-nix
      then toString ../. # for the case that we're in static-haskell-nix itself, so that CI always builds the latest version.
      # Update this hash to use a different `static-haskell-nix` version:
      else fetchTarball https://github.com/nh2/static-haskell-nix/archive/c49fbe730c49730d9568a9fec36f25f4580739f2.tar.gz;

  # Pin nixpkgs version
  # By default to the one `static-haskell-nix` provides, but you may also give
  # your own as long as it has the necessary patches, using e.g.
  #     pkgs = import (fetchTarball https://github.com/nh2/nixpkgs/archive/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa123.tar.gz) {};
  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = "2020-01-03T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  static_package = static-stack2nix-builder.static_package.overrideAttrs (old: {
    # Hack on static libraries the package needs that are, for reasons not yet
    # clear, not propagated automatically via the `cairo` Haskell library and
    # its `pkgconfig-depends` flag.
    # See https://github.com/nh2/static-haskell-nix/issues/47.
    # It's annoying that we have to do this with NIX_LDFLAGS, which is the
    # lowest-level hack via which one can insert linker flags, but we haven't
    # found higher level entry points yet that work.
    # Cabal's `--ld-options` might be one, but it doesn't work yet because
    # of GHC not passing them on in the correct order:
    # https://gitlab.haskell.org/ghc/ghc/issues/17071
    preConfigure = ''
     ${old.preConfigure or ""}
     export NIX_LDFLAGS="-lcairo -lfontconfig -lfreetype -lpixman-1 -lexpat -lpng $NIX_LDFLAGS"
   '';
  });

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

in
  {
    inherit static_package;
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
