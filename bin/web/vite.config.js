//import devtoolsJson from 'vite-plugin-devtools-json';
import tailwindcss from '@tailwindcss/vite';
import { defineConfig } from 'vitest/config';
import { sveltekit } from '@sveltejs/kit/vite';
import { viteCommonjs } from '@originjs/vite-plugin-commonjs'
//import { nodePolyfills } from 'vite-plugin-node-polyfills'


export default defineConfig({
    // see: https://github.com/sveltejs/kit/issues/7805#issuecomment-3514384288
    //      https://bugs.webkit.org/show_bug.cgi?id=242740
    //      https://github.com/sveltejs/kit/blob/72f77f57564fca41001224456e6c0eca28fc21b8/packages/kit/src/runtime/client/client.js#L768
    // build: {
    //     rollupOptions: {
    //         output: {
    //             inlineDynamicImports: true
    //         }
    //     }
    // },
	plugins: [
		tailwindcss(),
		sveltekit(),
		// devtoolsJson(),
        viteCommonjs(),
        //nodePolyfills(),
	],
	test: {
		expect: { requireAssertions: true },
		projects: [
			{
				extends: './vite.config.js',
				test: {
					name: 'client',
					environment: 'browser',
					browser: {
						enabled: true,
						provider: 'playwright',
						instances: [{ browser: 'chromium' }]
					},
					include: ['src/**/*.svelte.{test,spec}.{js,ts}'],
					exclude: ['src/lib/server/**'],
					setupFiles: ['./vitest-setup-client.js']
				}
			},
			{
				extends: './vite.config.js',
				test: {
					name: 'server',
					environment: 'node',
					include: ['src/**/*.{test,spec}.{js,ts}'],
					exclude: ['src/**/*.svelte.{test,spec}.{js,ts}']
				}
			}
		]
	}
});
