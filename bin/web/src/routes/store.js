import { default as eincalc } from '$lib/eincalcwrapper';
import { writable } from 'svelte/store';

// I actually have to do this - Thanks OCaml ??!!!
export var controller = writable({ 
    myLib:    eincalc['default'],
    refresh:  0,
    active :  '',
})
