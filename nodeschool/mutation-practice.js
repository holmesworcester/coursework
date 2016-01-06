// a function that multiplies every number in a list by 2, async style.
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

// this is vanilla.

console.log(multiplyAllMutation([1,2,3,4]))
console.log(times2MutationSafeTake2([1,2,3,4]))
console.log(times2Mutation([1,2,3,4]))
console.log(times2MutationSafe([1,2,3,4]))

// this shows some interesting behavior

console.log(multiplyAllMutation(aList))
console.log(times2MutationSafeTake2(aList))
console.log(times2Mutation(aList))
console.log(times2MutationSafe(aList))

// this shows some more interesting behavior. one reason why writing tests is more complicated than it seemed to me initially.

if (times2MutationSafeTake2(aList) == [2, 4, 6, 8]) console.log("times2MutationSafeTake2 works: [2, 4, 6, 8]") 
	else console.log("times2MutationSafeTake2 is broken: " + times2MutationSafeTake2(aList)) // will this be better?  NOPE!!

if (multiplyAllMutation(aList) == 24) console.log("multiplyAllMutation works: 24") 
	else console.log("multiplyAllMutation is broken: " + multiplyAllMutation(aList))

console.log(times2Mutation(aList))

if (times2Mutation(aList) == [2, 4, 6, 8]) console.log("times2Mutation works: [2, 4, 6, 8]") 
	else console.log("times2Mutation is broken: " + times2Mutation(aList)) // what happens here is so fucked up. i get it, but it's so fucked up :) good lesson in mutation!!

if (times2MutationSafe(aList) == [2, 4, 6, 8]) console.log("times2MutationSafe works: [2, 4, 6, 8]") 
	else console.log("times2MutationSafe is broken: " + times2MutationSafe(aList)) // will this be better?  NOPE!!



