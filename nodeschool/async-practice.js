// a function that multiplies every number in a list by 2, async style.
var _ = require('underscore')
var aList = [1, 2, 3, 4]

// List-of-Numbers -> Number
// Multiplies a list of numbers together. If the list is empty, it returns 1.

function multiplyAll (lon) {
	if (_.isEmpty(lon)) return 1
		else return (_.first(lon) * multiplyAll(_.rest(lon)))
}

// List-of-Numbers -> Number
// Multiplies a list of numbers together and can be called asynchronously. 
// If the list is empty, it returns 1.

function multiplyAsync (lon, callback) {
	if (_.isEmpty(lon)) callback(1)
		else multiplyAsync (_.rest(lon), function (number) {_.first(lon) * number})
}

// Number, Number, [Number -> X] -> []
// multiples two numbers together and calls callback on the result

function timesAsync (n1, n2, callback) {
	callback(n1 * n2)
}

// List-of-Numbers, [Number -> X] -> []
// Multiplies a list of numbers together asynchronously, using an asynchronous function for multiplication
// if the list is empty, it returns 1.

function multiplyRealAsync (lon, callbackConsumesNumber) {
	if (_.isEmpty(lon)) callbackConsumesNumber(1)
		else  multiplyRealAsync (_.rest(lon), function (number) {
			timesAsync(_.first(lon), number, callbackConsumesNumber)})
}

// [List-of X], Y, [X, Y, [Y -> Any]-> []], [Y -> Any] -> []
// Consumes a list of X, an initial Y, and an asynchronous function that consumes a X, a Y and a callback function that consumes Y.
// Takes the first item of X, combines it with Y using the provided function, and carries on the same operation with the next item in X and the product of the last operation.
// WORKED ON THE FIRST TRY! WOOT!


function myReduceAsync (lon, init, async2param, callback) {
	if (_.isEmpty(lon)) callback(init)
		else myReduceAsync (_.rest(lon), init, async2param, function (y) {
			async2param(_.first(lon), y, callback)
		})
}

// List-of-Numbers, [Number -> X] -> []
// Multiplies a list of numbers together asynchronously, using an asynchronous function for multiplication
// if the list is empty, it returns 1.
// uses myReduceAsync.

function multiplyAsyncAbstract (lon, callback) {
	myReduceAsync (lon, 1, timesAsync, callback)
}

// List-of-Numbers, [Number -> X] -> []
// Adds a list of numbers together asynchronously, using an asynchronous function for addition
// if the list is empty, it returns 0.
// uses myReduceAsync.

function addAsyncAbstract (lon, callback) {
	myReduceAsync (lon, 0, function (x, y, callback) {callback(x+y)}, callback)
}


// console.log(multiplyAll(aList))

multiplyAsyncAbstract (aList, console.log)
addAsyncAbstract (aList, console.log)
