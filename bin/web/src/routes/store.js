import { writable } from 'svelte/store';
var eincalc = await import('$lib/eincalc');

// for some reason loading the eincalc library from here breaks safari ????
// So we set it on mount from layout or page
// ?????

// I actually have to do this - Thanks OCaml ??!!!
export var controller = writable({ 
    /** @type object|null */
    myLib:    eincalc['default'], 
    refresh:  0,
    active :  '',
})
