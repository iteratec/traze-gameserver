# Traze Gameserver

This repository contains a server implementation for traze, a tron like game that can be played by sending mqtt messages.

### Technology stack

This software is written in `Haskell`, a purely functional programming language. It uses the `stack` build tool for compilation. We provide built binaries as docker images.

## Running the Server

This section will explain how to run the traze game server

The easiest way to run the traze gameserver yourself is to use docker. We provide pre built docker images via [docker hub](https://hub.docker.com/r/iteratec/traze-gameserver/)

```
docker run iteratec/traze-gameserver
```

you can override the default configuration using environment variables like so:

```
docker run \
  -e "TRAZE_BROKER_USER=<yourUserName>" \
  -e "TRAZE_BROKER_PASSWORD=<yourPassword>"\
  iteratec/traze-gameserver
```

### Connfiguration

This section explains the configuration of the traze gameserver. It can be configured externally using the environment variables listed below:

| Environment Variable  | Description                                   | Mandatory | Default Value |
| --------------------- | --------------------------------------------- | --------- | ------------- |
| TRAZE_BROKER_HOST     | The hostname of the MQTT Broker to connect to | No        | localhost     |
| TRAZE_BROKER_PORT     | The port of the MQTT Broker to connect to     | No        | 1883          |
| TRAZE_BROKER_USER     | The username for MQTT Broker authentication   | No        |               |
| TRAZE_BROKER_PASSWORD | The password for MQTT Broker authentication   | No        |               |
| TRAZE_INSTANCES       | A space separated list of instance names      | No        | "1"           |

## Contribute

This section aims to help you getting started to contribute to the traze game server. 

#### Building from Source

Traze uses [the haskell stack tool](https://docs.haskellstack.org/en/stable/README/). It will download a fitting compiler environment for you.
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

#### Building the Docker Image

You can build the docker image yourself in the project root directory.;
```
docker build -t traze-gameserver .
```

#### Docker Dev Setup

We also provide the possibility to run the haskell stack inside of docker. That way you do not have to install the development dependencies on your local machine. For easy usage we have provided the `Dockerfile.dev` and the helper script `run-docker-dev.sh`. To get started just run
```
./run-docker-dev.sh
```
This will setup a Docker container with all build tools ready to use and the sources mounted as a volume. You will end up within a bash session inside the working directory. For more options try
```
./run-docker-dev.sh --help
```

