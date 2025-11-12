<script>
	import '../app.css';
    // NB: had to change the URL option in the css file!
    import '$lib/assets/font-awesome.css';
	import favicon from '$lib/assets/favicon.svg';
    import kookaburra from '$lib/assets/Kookaboora.jpeg'
    import github from '$lib/assets/github-mark.svg'
	
	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
    <!--<link rel="stylesheet" href="/fonts/fontawesome-webfont.woff">-->
</svelte:head>

<div class="min-h-full" style="font-family: Outfit;">
    <nav class="navbar px-0 py-0 sticky top-0 bg-white z-50 flex text-center">
        <div class="min-w-full flex flex-col">
            <div class="flex h-16 items-start justify-between  border-b border-b-black">

                <div class="flex items-center h-16 px-8">
                    <div class="shrink-0 rounded">
                        <img src={kookaburra} alt="logo" 
                            class="size-6 border rounded-full object-cover
                            scale-150" />
                    </div>
                    <div class="flex flex-row">
                        <div class="ml-6 flex flex-col items-start justify-around space-x-4">
                            <div class="flex flex-row items-center"> 
                                <span class="text-black text-2xl font-bold">Eincalc</span>
                                <div class="badge badge-warning badge-xs
                                    font-bold mx-2 mt-1 tooltip tooltip-bottom"
                                    data-tip="Some behaviour may change in future releases">
                                    <svg class="size-[1em]" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 18 18"><g fill="currentColor"><path d="M7.638,3.495L2.213,12.891c-.605,1.048,.151,2.359,1.362,2.359H14.425c1.211,0,1.967-1.31,1.362-2.359L10.362,3.495c-.605-1.048-2.119-1.048-2.724,0Z" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"></path><line x1="9" y1="6.5" x2="9" y2="10" fill="none" stroke="currentColor" stroke-linecap="round" stroke-linejoin="round" stroke-width="1.5"></line><path d="M9,13.569c-.552,0-1-.449-1-1s.448-1,1-1,1,.449,1,1-.448,1-1,1Z" fill="currentColor" data-stroke="none" stroke="none"></path></g></svg>
                                    PRE-ALPHA
                                </div>
                            </div> 
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
                    <span class="text-xs font-thin text-center items-center w-16 p-4"> 
                       <i class="fa fa-star fa-xs" aria-hidden="true"></i> on Github 
                    </span>
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
