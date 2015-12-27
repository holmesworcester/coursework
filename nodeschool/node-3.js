var fs = require('fs');
const filename = process.argv[2];

function countLinesString (string) {
	return string.split('\n').length - 1;
}

function countLinesFile (err, data) {
	if (err) console.error(err) 
		else console.log(countLinesString(data.toString('utf-8')))
};

fs.readFile(filename, countLinesFile);
