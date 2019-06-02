{ pkgs }: {
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
  # 0.4 version is not released yet.
  # TODO: upgrade to latest.
  algebraic-graphs = pkgs.fetchFromGitHub {
    owner = "snowleopard";
    repo = "alga";
    rev = "1754312";
    sha256 = "09gp4vgslcr9r5w8h5jm1la0c5fnzxmhiv530rq7i16sisfrj4rr";
  };
  propertygraph-core = ./core;
}
