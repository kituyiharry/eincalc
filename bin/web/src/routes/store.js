import { writable } from 'svelte/store';
var eincalc = await import('$lib/eincalc');

// I actually have to do this - Thanks OCaml ??!!!
export var controller = writable({ 
    /** @type object|null */
    myLib:    eincalc['default'], 
    refresh:  0,
    active :  '',
})
