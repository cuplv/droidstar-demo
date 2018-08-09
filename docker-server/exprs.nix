with import <nixpkgs> {};


{

  sbt-android = stdenv.mkDerivation {
    name = "sbt-android";
    src = ./.;
    buildInputs = [ makeWrapper ];
    propagatedBuildInputs = [ androidsdk ];
    installPhase = ''
      mkdir -p $out/bin
      makeWrapper ${sbt}/bin/sbt $out/bin/sbt \
        --set ANDROID_HOME "${androidsdk}/libexec"
    '';
  };

  droidstar-demo-server = haskellPackages.callPackage ./server.nix {};

}
