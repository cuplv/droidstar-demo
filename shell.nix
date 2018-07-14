with (import <nixpkgs> {});

stdenv.mkDerivation {
  name = "droidstar-demo-setup";
  src = ./.;
  buildInputs = [ elmPackages.elm androidsdk ];
}
