# Traze Gameserver

This repository contains a server implementation for traze, a tron like game that can be played by sending mqtt messages.

## Contribute
This section aims to help you getting started to contribute to the traze game server. 

### Technology stack
This software is written in `Haskell`, a purely functional programming language. It uses the `stack` build tool for compilation. 

If you just want to run the game server locally without setting up a Haskell development environment you can do so by using Docker. You can either pull the image from the registry or build it yourself in the project root directory.
```
docker build
```

#### Docker Dev Setup
If you want to make changes to the source code you will want to build the software. For easy usage we have provided the `Dockerfile.dev` and the helper script `run-docker-dev.sh`. To get started just run
```
./run-docker-dev.sh
```
This will setup a Docker container with all build tools ready to use and the sources mounted as a volume. You will end up within a bash session inside the working directory. For more options try
```
./run-docker-dev.sh --help
```

#### Local Dev Setup
If you don't like docker you can run the dev tools on your host machine. For that you just need to install the stack tool. It will download a fitting compiler environment for you.
For MQTT connectivity we use language bindings to libmosquitto, an MQTT client library written in C. That is because the available native MQTT libraries in haskell come without TLS/SSL support at this point. You will need to install the library locally in order to be able to link the game server against it.
On Debian:
```
sudo apt-get install libmosquitto-dev
```
on Fedora:
```
sudo dnf install mosquitto-devel
```

Once you have installed stack and the required low level binaries, you can build the binaries by issuing the following command from the project root directory.

```
stack build
```

To run the test suite use `stack test`. You can also have stack create an interactive _read evaluate print loop_ environment, the so called _REPL_ for you with `stack ghci`.
