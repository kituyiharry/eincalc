// Actually has to be done like this:
import { writable } from 'svelte/store';

// See: <https://discuss.ocaml.org/t/creating-a-library-for-use-from-js-with-js-of-ocaml/9523/4>
//var eincalc = await import('$lib/eincalc');
var eincalc = await import('$lib/eincalc');
export default eincalc; 
