var express = require('express');
var request = require('request');
var bodyParser = require('body-parser');
var elmCompiler = require('node-elm-compiler');
var app = express();

app.get("/", function (req, res) {
  res.sendFile(__dirname + '/index.html');
});

var parser = bodyParser.raw({ type: "*/*" });
app.post("/proxy", parser, function (req, res) {
  request.post({
    url: "https://public-api.adsbexchange.com/VirtualRadar/AircraftList.json",
    body: req.body,
    qs: req.query,
    headers: { "Content-Type": "application/x-www-form-urlencoded" }
  }, function (err, msg, body) {
    if (err === null) {
      res.send(body);
    } else {
      res.sendStatus(500);
    }
  })
});

elmCompiler.compile(
  ["./ADSBApp.elm"],
  { yes: true, output: "index.html" })
  .on('close', function (compilerError) {
    if (compilerError === 0) {
      var listener = app.listen(3000, function () {
        console.log('Your app is listening on port ' + listener.address().port);
      });
    } else {
      console.error("Elm compiler returned error " + compilerError);
    }
  });


