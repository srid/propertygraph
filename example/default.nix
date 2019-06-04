let
  pkgs = import <nixpkgs> { };
  compilerVersion = "ghc844";  # TODO: Use the ghc from reflex-platform (obelisk) for cache benefits
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  root = ./.;
  source-overrides = import ../overrides.nix { inherit pkgs; };

  overrides = self: super: with pkgs.haskell.lib; {
    algebraic-graphs = dontCheck super.algebraic-graphs;
  };
}
