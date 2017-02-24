# Doppler GHCJS

Doppler is a simple Haskell library that provides an easy and typesafe way to
render and update DOM trees in Javascript. Doppler is inspired by React and
similar libraries, but it has the following advantages compared to other only
Javascript based solutions:

- Doppler is Haskell. You can enjoy the expressiviness and safety that Haskell
provides.

- Your HTML and CSS is checked at compile time.

- Doppler is lightweight; it only depends on [virtual-dom](https://github.com/Matt-Esch/virtual-dom)
and [ev-store](https://github.com/Raynos/ev-store) Javascript libraries.

- Event system allows you to update state everywhere and the DOM is atomically
updated to reflect the changes. Don't worry about multiple state updates,
only the most recent update is applied to DOM.

- Flexible syntax for HTML and CSS that allows variable interpolation -- all in
compile time.

- Encourages writing standalone components that contain HTML and CSS in one
place.

- No additional dependencies: just write your domain code in Haskell and
let Doppler to take care of showing it in browser. It really is that simple,
just take a look at samples.


## How to build?

Project depends on `doppler-html`, `doppler-css` and `doppler-event`.
The main project (`doppler-ghcjs`) contains only GHCJS depedency sources.
This is done to allow us to build and test core functionality of the library
without GHCJS dependency. If you don't have [GHCJS](https://github.com/ghcjs/ghcjs)
installed, please consult to GHCJS documentation for details.

To build the project, we use Cabal sandboxes. First, pull and configure
sandboxes for dependency projects.

```
cd {doppler-html, doppler-css, doppler-event}
cabal sandbox init --sandbox ../.doppler-sandbox
```

Now we can create and configure a sandbox for the main project.

```
cd doppler-ghcjs
cabal sandbox init
cabal sandbox add-source ../doppler-html
cabal sandbox add-source ../doppler-css
cabal sandbox add-source ../doppler-event
cabal install --only-dep
cabal configure
```

You can then build the main project.

```
cabal build
```

This will build the library and all samples. You can find samples inside `dist/build`
folder with `*.jsexe` ending.
