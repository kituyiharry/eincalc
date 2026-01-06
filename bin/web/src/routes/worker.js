// @type Promise<any>

//TODO: handle all eincalc operations from this thread!
//Come up with a token system for function calls that can be referenced back
//from the main thread!
var eincalc = import('$lib/eincalc');

self.onmessage = (code) => {
    console.log("we go it!!??", code, eincalc.then(function(d){
        (d['default'].executecodeasync(code.data, console.dir));
        self.postMessage(true);
    }));
}
