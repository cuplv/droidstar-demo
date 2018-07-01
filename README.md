# DroidStar Web Demo #

To build the docker images for client and server, you will need
`cabal`, `elm`, and `sbt`.  Running `make` will produce both images.

The running system requires three containers in all:

1. An android emulator image, with port link `5555:5555`
2. The server image, with port link `3000:3000`.
3. The client image, with port link `8080:80`

The client can then be reached on `localhost:8080`.
