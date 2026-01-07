//TODO: handle all eincalc operations from this thread!
//Come up with a token system for function calls that can be referenced back
//from the main thread!
import { writable } from 'svelte/store';
var eincalc = import('$lib/eincalc');

var controller = writable({
    /** @type object|null */
    myLib: null 
});

eincalc.then(function(d) {
    console.log("updating!!");
    controller.update(function(old){
        if (old.myLib) return;
        old.myLib = d['default']
        console.dir("instance!!");
        return old;
    });
}).catch(console.error);

self.onmessage = (code) => {
    console.log("we go it!!??", code);
    controller.update(function(d){
        console.log(d.myLib?.formulaes(), d);
        d.myLib?.executecode(code.data);
        return d;
    });
    self.postMessage(code.data);
}
