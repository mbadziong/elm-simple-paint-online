var express = require('express');
var app = express();
var expressWS = require('express-ws')(app);

var portNumber = 1234;
var lines = [];

app.listen(portNumber, function() {
  console.log(`Listening on port ${portNumber}`);
});

var aWss = expressWS.getWss('/test');

app.ws('/test', function(websocket, request) {
  console.log('A client connected!');

  websocket.on('message', function(message) {
    lines.push(message);
    aWss.clients.forEach(function (client) {
      client.send(JSON.stringify(lines));
    });
  });
});