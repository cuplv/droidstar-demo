FROM nixos/nix

WORKDIR /root

RUN nix-channel --add https://nixos.org/channels/nixos-18.03 nixpkgs
RUN nix-channel --update

RUN nix-env -iA nixpkgs.graphviz
RUN nix-env -iA nixpkgs.cabal2nix
RUN nix-env -iA nixpkgs.androidsdk
RUN nix-env -iA nixpkgs.git

COPY ./_server ./server
COPY ./exprs.nix ./exprs.nix

RUN cabal2nix server > server.nix

RUN nix-env -f exprs.nix -iA sbt-android
RUN nix-env -f exprs.nix -iA droidstar-demo-server

RUN git clone https://github.com/cuplv/droidstar
RUN cd droidstar && git checkout demo-config && sbt driverApp/android:package

CMD ["droidstar-demo-server","--enable-custom","--init-delay","120"]
