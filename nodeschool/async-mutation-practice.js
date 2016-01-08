// a function that multiplies every number in a list by 2, async style.
var http = require('http')
var _ = require('underscore')
var aList = [1, 2, 3, 4]

// List-of-Numbers -> Number
// Multiplies a list of numbers together with a for loop and mutation. If the list is empty, it returns 1

function multiplyAllMutation (lon) {
	var product = 1
	for (i=0; i < lon.length; i++) {
		product = product * lon[i]
	} return product
}

// List-of-Numbers -> List-of-Numbers
// Multiplies a list of numbers by 2. 

function times2MutationSafeTake2 (lon) {
	var mutant2 = lon.slice() // now it's a shallow copy.
	for (i=0; i < mutant2.length; i++) {
		mutant2[i] = mutant2[i] * 2
	} return mutant2
}

// List-of-Numbers -> List-of-Numbers
// Multiplies a list of numbers by 2. 

function times2Mutation (lon) {
	for (i=0; i < lon.length; i++) {
		lon[i] = lon[i] * 2
	} return lon
}

// List-of-Numbers -> List-of-Numbers
// Multiplies a list of numbers by 2. 

function times2MutationSafe (lon) {
	var mutant = lon // wow so this tells the compiler that the two are the **SAME**.
	for (i=0; i < mutant.length; i++) {
		mutant[i] = mutant[i] * 2
	} return mutant
}


// Write a function that does this async style but where operations can happen in parallel, but where it doesn't return them
// until they are all done. 

// List-of-Numbers, [List-of-Numbers -> []] -> []
// Multiplies a list of numbers by 2. 

function times2MutationAsync (lon, callback) {
	var callbackCount = 0
	for (i=0; i < lon.length; i++) timesAsync (lon[i], 2, function (product) {
				lon[i] = product
				callbackCount ++
			});
		do 
		{console.log(".")} 
		while (callbackCount < lon.length)
	callback(lon)
}

// Make an abstract map function that works this way.

// [List-of X], [X, [Y -> []]]] -> []], Y]   --- woh is this right??
// a map function that works asynchronously and takes in an async function. 

function asyncMap (list, async1ParamFunc, callback) {
		var callbackCount = 0
		for (i=0; i < list.length; i++) async1ParamFunc (list[i], function (result) {
				list[i] = result
				callbackCount ++
			});
		do 
		{} 
		while (callbackCount < list.length) // is there a less stupid way to wait for callbacks to complete?
	callback(list)
}


function times2FromMap (lon, callback) {
	// consumes a number and a callback, runs the callback on the number times 2.
	function times2Async (number, timescallback) {
		timesAsync (number, 2, timescallback)
	} 
	// in
	asyncMap (lon, times2Async, callback)
}


// Number, Number, [Number -> X] -> []
// multiples two numbers together and calls callback on the result
// If the first number is odd, it does unnecessary activity to simulate a function where there were actually strange delays and tasks completed out of order.

function timesAsync (n1, n2, callback) {
	if (n1 % 2) {
		for (j=0; j < n1 * 10000; j++) {
		console.log(".")}
		callback(n1 * n2)
	} else {
		callback(n1 * n2)
	}
}


// times2MutationAsync([1,2,3,4], console.log)
times2FromMap([1,2,3,4], console.log)
// asyncMap (["http://twitter.com/", "http://google.com"], http.get, console.log)