## NB: When running the web

- make sure the commonjs vite transform and node polyfills are available - just
  in case - so that objects created in Ocaml can be available to you in the
  browser - otherwise it seems vite will run into issues and badly transform your
  bundle. They should have already been configured for this project
- you MUST build the project in RELEASE mode in order for it to work in the
  browser because JSOO or Vite messes up something when sourcemaps are there!
- May need to do additional work with jsdoc in order to get types in the svelte
  side
- You will need to add a flag for effects if needed in the future
  see: <https://ocsigen.org/js_of_ocaml/dev/manual/options>
