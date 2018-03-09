# traze

This repository contains a tron like game that can be played by sending mqtt messages to the server. 

## How to play?
The communication with the game server works via an MQTT message broker. There are different message types your client has to support in order to participate in the game.

### Select an Game Instance
You can query currently running Games. 

`traze/games`
```json
[
    {
        "name": "instanceName",
        "activePlayers": 5
    }
]

```

### Spectate a Game

#### MQTT 
If you want to write an ai pilot you can do so by parsing the MQTT repersentation of the grid. It is published on the MQTT Topic

`traze/{instanceName}/grid`
```json
{
  "height":3,
  "width":3,
  "tiles":[
        [ 1, 0, 0 ],
        [ 1, 1, 2 ],
        [ 0, 2, 2 ]
    ],
  "bikes":[
      {
        "playerId":2,
        "currentLocation":[1,0],
        "direction":"W",
        "trail":[[2,0],[2,1]]
        }
   ],
   "spawns":[[2,2]]
}
```


In addition to the grid you might receive a list of currently active players.

`traze/{instanceName}/players`
```json
[
   {
     "id": 1,
     "name": "player1",
     "color": "#28BA3C",
     "frags": 1,
     "owned": 2
   },
   {
     "id": 2,
     "name": "player2",
     "color": "#0A94FF",
     "frags": 2,
     "owned": 1
   }
]
```

Finally there is a ticker topic that informs about frags that occoured on a given instance.
`traze/{instanceName}/ticker`
```json
{
  "type": "frag",
  "casualty": 2,
  "fragger": 4
}
```
The types are of `frag`, `collision`, `suicide`.

### Play the game

#### Client Registration
You send a request to join the game. In return you'll get a user token that allows you to control your bike. The Response will be sent to your private MQTT topic.

`/traze/{instanceName}/join`
```json
"name": "myIngameNick"
```

If the server accepts your request you'll receive a message communicating your initial position. Once you give your first direction command your game starts.

`traze/{instanceName}/player/{myIngameNick}`
```json
{
    "id": 1337,
    "name": "myIngameNick",
    "secretUserToken":"de37c1bc-d0e6-4c66-aaa3-911511f43d54",
    "position": [15,3]
}
```
Because the ingame nick is part of the topic your nickname may not include `#`, `+`, `/`.

#### Steering your Light Cycle
You steer by giving the directions for your next turn via an MQTT message. If you don't commit a course correction within the specified timeframe your light cycle will continue on it's previous path.

`traze/{instanceName}/{playerId}/steer`
```json
{
    "course":"N",
    "playerToken": "de37c1bc-d0e6-4c66-aaa3-911511f43d54"
}
```

The options for a course Change are North, South, East or West. 

#### Leaving a Game
You may leave the game at any time.

`traze/{instanceName}/{playerId}/bail`
```json
"playerToken": "yourSecretToken"
```

## Development
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
