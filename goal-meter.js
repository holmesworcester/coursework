signatures = process.argv[2]

// Consumes a number of signatures and returns a goal number

function nextRoundNumber (sigs) {
	var buffer = 1.2 // how close we let it get to goal. 
	var roundNumbers = [0, 100, 250, 500, 1000, 1500, 2000, 2500, 5000, 7500, 10000, 15000, 20000, 25000, 50000, 75000, 100000, 150000, 200000, 250000, 500000, 750000, 1000000, 1500000, 200000, 5000000]
	for (i = 0; i < roundNumbers.length; i++) {
	if (((sigs*buffer) >= roundNumbers[i]) && (sigs*buffer) <= roundNumbers[i + 1]) {
		return roundNumbers[i + 1]
		} else return roundNumbers[roundNumbers.length - 1] // we let it max out, because i'm lazy :)
	}
}

console.log(nextRoundNumber(signatures))

// for (i=0; i < 100000; i = i + 27) {
//	console.log(nextRoundNumber(i))
// }