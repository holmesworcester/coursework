var fs = require('fs');
var path = require('path');

module.exports = lsFiltered; // exports a module

// String, String, Function -> ()
// consumes the directory name, the filename extension string and a callback function
// (one that consumes two parameters: err, data)
// runs the callback function on error and data depending on whether there's an error or not.

function lsFiltered(dirName, fileExtension, callback) {
	// a local function that uses fileExtension as a constant
	// and consumes an error and an array so that it can be a callback
	// to fs.readdir. produces error or an
	function filterSpecific (error, unfilteredList) {
		if (error) callback(error) // again, can i give just the error?
		else callback(false, unfilteredList.filter(function (file) {if (path.extname(file) === '.' + fileExtension) return true}));
	};
	fs.readdir(dirName, filterSpecific);
}

