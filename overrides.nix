{ pkgs }: {
  aeson-gadt-th = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "aeson-gadt-th";
    rev = "65df0b070dd21e4c2285fc485e5b9857dbc8ad5b";
    sha256 = "1h6wkag38r9xmf3yd0f64h0mfzb7cfj8cf7n2r5mqdyiqz3a5x4w";
  };
  constraints-extras = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "constraints-extras";
    rev = "30f10c03dd96e50c089f0613f99951805bff7397";
    sha256 = "196b8kbcp744gqhh964m54vw4cdg15p6lc7cm2vxbh15cbqdz7ir";
  };
  dependent-sum-aeson-orphans = pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "dependent-sum-aeson-orphans";
    rev = "198fb00b9307a1bd44dfae7abad75363576e9d69";
    sha256 = "19555ddi7lrwpra55qxcl67i9c3w9yhfnkbw0smb3ckz0yzvra0n";
  };

  acid-state = pkgs.fetchFromGitHub {
    owner = "acid-state";
    repo = "acid-state";
    rev = "bf1fa2466e749f91d2e3152ced15331f062e8d10";
    sha256 = "1jifqaf5c54s272a7pihfwpk4qp69d2zkv9hxplmfrhpzsi2yc5q";
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
  propertygraph-acid = ./acid;
}
