var http = require('http')
var bl = require('bl')
const url1 = process.argv[2]
const url2 = process.argv[3]
const url3 = process.argv[4]

// const urls = [url1, url2, url3]

// for counting completed callbacks
var urlsCompleted = 0

// run everything.
http.get(url1, getAndCount)
http.get(url2, getAndCount)
http.get(url3, getAndCount)

// consumes (data, err) and returns data from a request for one URL and contains something that
// increments the number of callbacks each time, only once all data is collected. 
function getAndCount (response) {
     response.pipe(bl(function (err, data) {
           urlsCompleted = urlsCompleted + 1; // increments
           			return data.toString;
 		}
	))
} 

while (urlsCompleted < 3) {
	2 * 5; // do whatever
} console.log('Acumulator works'); // this might need something to split up strings 
