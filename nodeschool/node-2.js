var params = process.argv;
var sum = 0;

for (i=2; i < params.length; i++) {
	sum += Number(params[i]);
};

console.log(sum);