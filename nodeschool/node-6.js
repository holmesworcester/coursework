var http = require('http')
const url = process.argv[2]

http.get(url, callback)

function callback(response){
	response.on("data", console.log).setEncoding('utf8')
}
   
 // interest are: "data", "error" and "end". You listen to an event like so:  
   
  //   response.on("data", function (data) { /* ... */ })  


// The response object / Stream that you get from http.get() also has a  
//  setEncoding() method.