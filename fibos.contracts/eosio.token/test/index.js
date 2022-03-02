var test = require('test');
test.setup();

run("./smart");
run("./classic");
run("./FO");
run("./nft");

test.run(console.DEBUG);