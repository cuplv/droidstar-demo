FROM alpine

RUN apk update
RUN echo "https://s3-us-west-2.amazonaws.com/alpine-ghc/8.0" >> /etc/apk/repositories
ADD https://raw.githubusercontent.com/mitchty/alpine-ghc/master/mitch.tishmack%40gmail.com-55881c97.rsa.pub \
    /etc/apk/keys/mitch.tishmack@gmail.com-55881c97.rsa.pub
RUN apk update
RUN apk add alpine-sdk git ca-certificates gmp-dev zlib-dev linux-headers musl-dev android-tools
RUN apk add ghc cabal
RUN cabal update

COPY ./server /root/server
COPY ./droidstar-debug.apk /root/droidstar-debug.apk

RUN cd /root/server && cabal install --only-dependencies && cabal configure && cabal build
RUN cp /root/server/dist/build/droidstar-demo-server/droidstar-demo-server /bin/droidstar-demo-server

CMD ["/bin/droidstar-demo-server"]
