<script>
	import '../app.css';
    import '$lib/assets/font-awesome.min.css';
	import favicon from '$lib/assets/favicon.svg';
    import kookaburra from '$lib/assets/Kookaboora.jpeg'
    import github from '$lib/assets/github-mark.svg'
	
	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

<div class="min-h-full">
    <nav class="navbar px-0 py-0 sticky top-0 bg-white z-50 flex text-center">
        <div class="min-w-full flex flex-col">
            <div class="flex h-16 items-start justify-between  border-b border-b-black">

                <div class="flex items-center h-16 px-8">
                    <div class="shrink-0 rounded">
                        <img src={kookaburra} alt="logo" 
                            class="size-6 border rounded-full object-cover
                            scale-150" />
                    </div>
                    <div class="">
                        <div class="ml-6 flex flex-col items-start justify-around space-x-4">
                            <span class="text-black text-3xl font-bold">Eincalc</span>
                            <span class="text-black text-xs font-thin">A
                                SpreadSheet for Shape Rotators! </span>
                            <!-- TODO: add other options here if needed -->
                        </div>
                    </div>
                </div>

                <div class="flex items-center h-16 px-8">
                    <div class="shrink-0 rounded">
                        <a target="_blank" href="https://github.com/kituyiharry/eincalc"> 
                            <img src={github} alt="logo" 
                            class="size-8 border rounded-full object-cover scale-130" />
                        </a>
                    </div>
                    <span class="text-xs font-thin text-center items-center w-16 p-4"> Star on Github </span>
                </div> 

                <!-- TODO: Add subscribe or integrate button -->
            </div>

            <header class="after:pointer-events-none after:absolute
                after:inset-x-0 after:inset-y-0 after:border-y after:border-white/10
                border-b border-b-black z-auto">
                <div class="pl-0">
                    <div role="tablist" class="pl-14 tabs "> 
                        <div role="tab" data-tip="Multiple sheets coming soon" 
                            class="tooltip tooltip-bottom outline-black tab tab-active px-2 border-x border-x-black">
                            <span class="badge text-sm badge-neutral badge-outline">
                                <i class="fa fa-circle text-green-200"> </i>
                                Default Worksheet
                            </span>
                        </div>
                    </div>
                </div>
            </header>
            <!-- Page content here -->
            {@render children?.()}
        </div>
    </nav>
</div>

<style> 
</style>
