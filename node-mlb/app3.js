var express = require('express');
var app = express();

app.get('/', function(req, res){
    res.send('OK, no problema!');
});

app.get('/tetra/:x', function(req, res){
    var x = parseInt(req.params.x);
    var result = Math.pow(x,x);
    res.send('' + result);
});


app.listen(3053);

console.log('app is listening at localhost:3000'); 
