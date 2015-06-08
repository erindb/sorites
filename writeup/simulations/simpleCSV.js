// simpleCSV.js

var fs = require('fs');
var babyparse = require('babyparse');

function readCSV(filename){
  return babyparse.parse(fs.readFileSync(filename, 'utf8'));
};

function writeCSV(jsonCSV, filename){
  fs.writeFileSync(filename, babyparse.unparse(jsonCSV) + "\n");
}

module.exports = {
  readCSV: readCSV,
  writeCSV: writeCSV
};