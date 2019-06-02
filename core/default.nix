let
  pkgs = import <nixpkgs> { };
  compilerVersion = "ghc844";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  root = ./.;
  source-overrides = {
    aeson-gadt-th = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "aeson-gadt-th";
      rev = "e40c293901a9cb9be4b0748109f4bc6806bfdb79";
      sha256 = "08iqyzd4240g7af2lwgxmqbfglyxxii43i2zi01xmk1kg8inzs3v";
    };
    constraints-extras = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "constraints-extras";
      rev = "f6a12f9403c77ae075029171fc572e7d26222bb7";
      sha256 = "1kflqgn5zcmd5f7z8m4z45xd0ybgh080qi9x9dkl15qsszrca6ry";
    };
    dependent-sum-aeson-orphans = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "dependent-sum-aeson-orphans";
      rev = "75d7f07b5ac357fb20494f98f6d865b78ee44b06";
      sha256 = "1wx4l1mgyplcpczj7n79vf3p0n1fbbfzvh17nw3cqf4rscvp8y0f";
    };
  };
}
