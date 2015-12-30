var http = require('http')
var _ = require('underscore')

const url1 = process.argv[2]
const url2 = process.argv[3]
const url3 = process.argv[4]

const urls = [url1, url2, url3]


function doOneURL(url, callbackforOne) {
	http.get(url, callbackforOne)	
}

// takes in a URL and calls the function callbackforOne on the response
// given by httpget, which is data.
// now I just don't know how to get the output into the right format.

function doOneURLasOneLineString(url, callbackforOne) {
	http.get(url, function callback(response) {
	response.on("data", callbackforOne).setEncoding('utf8')})
}

// List-of-URLs -> Data

function doListofURLs(urlList, next) {
	if (_.isEmpty(urlList)) next([]) // this is the line I don't really understand. 
		// then we are done and I should return no data.
		// the way I return things is with the callback.
		else 
		// there is more data to do
		// I should process the first item of urlList qith doOneURL
		// and compose it with a recursive use of the doListofURLs function.
		// remember I am "returning" things with a callback
		doOneURL(_.first(urlList), function (responseFirst) {
			doListofURLs(_.rest(urlList), function (responseRest) {
				next(responseFirst.data + "\n" + responseRest.data) // can't figure out how to turn these into strings.
			})
		})
	}

doListofURLs(urls, function (everything) {
	console.log(everything)})