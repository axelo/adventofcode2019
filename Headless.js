const { Elm } = require("./dist/headless.js");

const [_a, _b, ...days] = process.argv;

const headless = Elm.Headless.init({
  flags: days
});

headless.ports.sendSolutionOutput.subscribe(console.log);
