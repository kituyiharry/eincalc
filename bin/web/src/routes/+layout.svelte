<script>
	import '../app.css';
	import favicon from '$lib/assets/favicon.svg';
    import kookaburra from '$lib/assets/Kookaboora.jpeg'
	
	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

<div class="min-h-full">
    <nav class="navbar px-0 py-0 sticky top-0 bg-white z-50 flex text-center">
        <div class="min-w-full flex flex-col">
            <div class="flex h-16 items-start justify-between  border-b border-b-black">

                <div class="flex items-center px-8">
                    <div class="shrink-0 rounded">
                        <img src={kookaburra} alt="logo" 
                            class="size-6 border rounded-full object-cover scale-130" />
                    </div>
                    <div class="">
                        <div class="ml-6 flex items-baseline space-x-4">
                            <span class="text-black text-3xl py-2 font-bold">Eincalc</span>
                            <!-- TODO: add other options here if needed -->
                        </div>
                    </div>
                </div>
                <!-- TODO: Add subscribe or integrate button -->
            </div>

            <header class="after:pointer-events-none after:absolute
                after:inset-x-0 after:inset-y-0 after:border-y after:border-white/10
                border-b border-b-black z-10">
                <div class="pl-0 z-10">
                    <div role="tablist" class="pl-16 tabs tabs-border"> 
                        <div role="tab" class="outline-black tab tab-active">
                            <span class="badge text-sm badge-neutral badge-outline">
                                <svg class="size-[1em]" xmlns="http://www.w3.org/2000/svg" 
                                    viewBox="0 0 24 24"><g fill="currentColor" stroke-linejoin="miter" stroke-linecap="butt">
                                        <circle cx="12" cy="12" r="10" fill="green"
                                            stroke="currentColor" stroke-linecap="square" stroke-miterlimit="10" stroke-width="2">
                                        </circle></g>
                                </svg>
                                Sheet A
                            </span>
                        </div>
                    </div>
                </div>
            </header>

        </div>
    </nav>
    <div class="drawer fixed drawer-open">
        <input id="my-drawer-4" type="checkbox" class="drawer-toggle" />
        <div class="drawer-content">
            <!-- Page content here -->
            {@render children?.()}
        </div>

        <div class="drawer-side is-drawer-close:overflow-visible  border-r border-r-black">
            <label for="my-drawer-4" aria-label="close sidebar" class="drawer-overlay"></label>
            <div class="is-drawer-close:w-14 is-drawer-open:w-64 bg-base-200 flex flex-col items-start min-h-full">
                <!-- Sidebar content here -->
                <ul class="menu w-full grow">

                    <!-- list item -->
                    <li>
                        <button class="is-drawer-close:tooltip is-drawer-close:tooltip-right" data-tip="Homepage">
                            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" stroke-linejoin="round" stroke-linecap="round" stroke-width="2" fill="none" stroke="currentColor" class="inline-block size-4 my-1.5"><path d="M15 21v-8a1 1 0 0 0-1-1h-4a1 1 0 0 0-1 1v8"></path><path d="M3 10a2 2 0 0 1 .709-1.528l7-5.999a2 2 0 0 1 2.582 0l7 5.999A2 2 0 0 1 21 10v9a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"></path></svg>
                            <span class="is-drawer-close:hidden">Homepage</span>
                        </button>
                    </li>

                </ul>
            </div>
        </div>
    </div>
</div>

<style> 
</style>
