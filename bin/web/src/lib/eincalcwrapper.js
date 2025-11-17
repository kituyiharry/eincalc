// Actually has to be done like this:
// See: <https://discuss.ocaml.org/t/creating-a-library-for-use-from-js-with-js-of-ocaml/9523/4>
var eincalc = await import('$lib/eincalc');
//var eincalcwasm = require('$lib/eincalcwasm');
//import * as eincalcwasm from '$lib/eincalcwasm';
export default eincalc;
