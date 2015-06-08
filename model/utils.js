// utils.js

var fs = require('fs');
var numero = require('numero');
var babyparse = require('babyparse');

function readCSV(filename){
  return babyparse.parse(fs.readFileSync(filename, 'utf8'));
};

function writeCSV(jsonCSV, filename){
  fs.writeFileSync(filename, babyparse.unparse(jsonCSV) + "\n");
}

function float(str) {
	if (str[0] == "0" & str.length > 1) {
		return numero.parseFloat(str.slice(1, str.length));
	} else {
		return numero.parseFloat(str);
	}
}

module.exports = {
  readCSV: readCSV,
  writeCSV: writeCSV,
  float: float
};