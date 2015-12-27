var fs = require('fs');
var path = require('path');
const pathName = process.argv[2];
const fileExtension = process.argv[3];

fs.readdir(pathName, printFilteredFiles);

// Object, Array -> (console)
// takes in an error object and an array (list of files) and returns error if error is truthy.
// if no error, logs the list of files to the console filtered by fileFilter.
// they must come out one per line.

function printFilteredFiles(error, filesArray) {
	if (error) console.error(error) 
		else printFiles(fileFilter(filesArray));
}

// List-of-Files -> List-of-Files
// filters a List-of-Files (and array) by the constant fileExtension such that only *.fileExtension files appear.

function fileFilter (files) {
	return files.filter(function (file) {if (path.extname(file) === '.' + fileExtension) return true});
};

// List-of-Files -> (console)
// prints a list of files one per line

function printFiles (files) {
	files.forEach(function (item) {(console.log(item))});
}
