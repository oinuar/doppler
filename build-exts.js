// Convert everything to amd-modules:
//    node node_modules/requirejs/bin/r.js -convert node_modules amd_modules
// and then:
//    node node_modules/requirejs/bin/r.js -o build.js
// then we can combine everything:
//    cat ext.js all.js > app.js

({
   appUrl: "dist/build/doppler/doppler.jsexe/",
   baseUrl: "./amd_modules",
   out: "dist/build/doppler/doppler.jsexe/ext.js",
   optimize: "none",
   include: ["virtual-dom", "ev-store"],
   name: "../node_modules/almond/almond",
   packages: [
      {
         name: "virtual-dom",
         location: "virtual-dom/",
         main: "index"
      },
      {
         name: "x-is-array",
         location: "x-is-array/",
         main: "index"
      },
      {
         name: "is-object",
         location: "is-object/",
         main: "index"
      },
      {
         name: "min-document",
         location: "min-document/",
         main: "index"
      },
      {
         name: "dom-walk",
         location: "dom-walk/",
         main: "index"
      },
      {
         name: "browser-split",
         location: "browser-split/",
         main: "index"
      },
      {
         name: "ev-store",
         location: "ev-store/",
         main: "index"
      }
   ]
})
