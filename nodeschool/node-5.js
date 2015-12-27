// Same as node-4.js but putting everything into a module.

const pathName = process.argv[2];
const fileExtension = process.argv[3];

var lsFilter = require('./lsFilter.js');

// List-of-Files -> (console)
// prints a list of files one per line 

lsFilter(pathName, fileExtension, function (error, fileList) 
	{if (error) 
		return console.error(error)
		else fileList.forEach(function (item) {
			console.log(item)
		})
	}
);

