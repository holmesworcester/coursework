var http = require('http')
var bl = require('bl')
const url = process.argv[2]

http.get(url, callback)

function callback(response){
     response.pipe(bl(function (err, data) {
        if (err)  
           return console.error(err)  
     	const s = data.toString()
     	console.log(s.length)
     	console.log(s)
 		}
	))
} 

//	response.on("data", console.log).setEncoding('utf8')

   
 // interest are: "data", "error" and "end". You listen to an event like so:  
   
  //   response.on("data", function (data) { /* ... */ })  


// The response object / Stream that you get from http.get() also has a  
//  setEncoding() method.