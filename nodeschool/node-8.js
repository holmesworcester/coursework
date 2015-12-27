var http = require('http')
var bl = require('bl')
const url1 = process.argv[2]
const url2 = process.argv[3]
const url3 = process.argv[4]

const urls = [url1, url2, url3]

// for counting completed callbacks
var urlsCompleted = 0
// for storing the results
var allData = urls.map(function (url) {
	return http.get(url, getAndCount)}) // I think the problem is here. 

// consumes (data, err) and returns data from a request for one URL and contains something that
// increments the number of callbacks each time, only once all data is collected. 
function getAndCount (response) {
     response.pipe(bl(function (err, data) {
			return data.toString;
           urlsCompleted = urlsCompleted + 1; // increments
         if (urlsCompleted >= 3) console.log(allData.toString); // this might need something to split up strings 
 		}
	))
} 