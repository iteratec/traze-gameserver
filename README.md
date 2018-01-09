# hackaTron

This repository contains a tron like game that can be played by sending mqtt messages to the server. 

## How to play?
The communication with the game server works via an MQTT message broker. There are different message types your client has to support in order to participate in the game.

### Select an Game Instance
You can query currently running Games. 

TBD.

### Spectate a Game

#### Web UI
You can stream the UI onto your own web enabled device.

TBD.

#### MQTT 
If you want to write an ai pilot you can do so by parsing the MQTT repersentation of the grid. It is published 

### Client Registration
You send a request to join the game. In return you'll get a user token that allows you to control your bike. The Response will be sent to your private MQTT topic.

/hackaTron/{instanceName}/join
```json
request: {
    playerName: "yourFancyPublicUserHandle"
}
```

If the server accepts your request you'll receive a message communicating your initial position. Once you give your first direction command your game starts.

### Steering your Light Cycle
You steer by giving the directions for your next turn via an MQTT message. If you don't commit a course correction within the specified timeframe your light cycle will continue on it's previous path.

/hackaTron/{instanceName}/{playerId}/steer
```
course:"North"
```

The options for a course Change are North, South, East or West. 


