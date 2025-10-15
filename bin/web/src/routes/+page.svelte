<script>
    import * as data from '$lib/index.js';
    import { onMount } from 'svelte';
    let myNode;

    onMount(() => {
        if (myNode) {
            data.default.myLib.load(myNode);
        }
    });

</script>

<h1>Welcome to SvelteKit</h1>
<p>Visit <a href="https://svelte.dev/docs/kit">svelte.dev/docs/kit</a> to read the documentation</p>
<div id="spine" bind:this={myNode}>
</div>
