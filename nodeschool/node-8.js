var http = require('http')
var bl = require('bl')
const url1 = process.argv[2]
const url2 = process.argv[3]
const url3 = process.argv[4]

const urls = [url1, url2, url3]
const i = -1 // url index

// Consumes a response I can pass it as a callback to httpGet.

doAllRemainingURLs('starting this off!')

function doAllRemainingURLs (response) {
	console.log(response)
//	response.on("data", console.log).setEncoding('utf8')
	i = i + 1;
	if (i < 3) http.get(urls[i], doAllRemainingURLs)
}

