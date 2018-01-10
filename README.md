# hackaTron

This repository contains a tron like game that can be played by sending mqtt messages to the server. 

## How to play?
The communication with the game server works via an MQTT message broker. There are different message types your client has to support in order to participate in the game.

### Select an Game Instance
You can query currently running Games. 

hackaTron/games
```json
[
    {
        name: "instanceName",
        activePlayers: 5
    }
]

```

### Spectate a Game

#### MQTT 
If you want to write an ai pilot you can do so by parsing the MQTT repersentation of the grid. It is published on the MQTT Topic

hackaTron/{instanceName}/grid
```json
grid: {
    height: 3,
    width: 3,
    tiles:[
        [ 1, 0, 0 ],
        [ 1, 1, 2 ],
        [ 0, 2, 2 ]
    ]
}
```
In addition to the grid you might receive a list of currently active players.

hackaTron/{instanceName}/players
```json
currentPlayers: [
   {
     id: 1,
     name: "player1",
     color: "#28BA3C",
     frags: 1,
     owned: 2
   },
   {
     id: 2,
     name: "player2",
     color: "#0A94FF",
     frags: 2,
     owned: 1
   }
]

#### Web UI
You can stream the UI onto your own web enabled device.

TBD.

### Play the game

#### Client Registration
You send a request to join the game. In return you'll get a user token that allows you to control your bike. The Response will be sent to your private MQTT topic.

/hackaTron/{instanceName}/join
```json
name: "yourFancyPublicUserHandle"
```

If the server accepts your request you'll receive a message communicating your initial position. Once you give your first direction command your game starts.

hackaTron/{instanceName}/player/{playerName}
```json
you: {
    id: 1337,
    name: "yourFancyPublicUserHandle",
    secretUserToken:"",
    position: (15,3)
}
```

#### Steering your Light Cycle
You steer by giving the directions for your next turn via an MQTT message. If you don't commit a course correction within the specified timeframe your light cycle will continue on it's previous path.

hackaTron/{instanceName}/{playerId}/steer
```json
{
    course:"North",
    playerToken: "yourSecretToken"
}
```

The options for a course Change are North, South, East or West. 

#### Leaving a Game
You may leave the game at any time.

hackaTron/{instanceName}/{playerId}/bail
```json
playerToken: "yourSecretToken"
```
