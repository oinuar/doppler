# Doppler

Doppler is a simple Haskell library that provides an easy and typesafe way to
render and update DOM trees in Javascript. Doppler is inspired by React and
similar libraries, but it has the following adventiges compared to other only
Javascript based solutions:

- Doppler is Haskell. You can enjoy the expressiviness and safety that Haskell
provides.

- Your HTML and CSS is checked at compile time.

- Doppler is lightweight; it only depends on virtual-dom and ev-store Javascript
libraries.

- Event system allows you to update state everywhere and the DOM is atomically
updated to reflect the changes. Don't worry about multiple state updates,
only the most recent update is applied to DOM.

- Flexible syntax for HTML and CSS that allows variable interpolation -- all in
compile time.

- Encourages writing standalone components that contain HTML and CSS in one
place.


## How to build?

Project has two Cabal projects. Doppler-core contains all generic Haskell
source files. The main projet (doppler) contains only GHCJS depedency sources.
This is done to allow us to build and test core the functionality of the library
without GHCJS dependency. If you don't have GHCJS installed, please consult
to GHCJS documentation for details.

To build the project, we use Cabal sandboxes. First, a sandbox is created and
configured for the core project.

  cd core
  cabal sandbox init
  cabal install --enable-tests --only-dep
  cabal configure --enable-tests

Then you can run tests to ensure that the code works as expected.

  cabal test

Now we can create and configure a sandbox for the main project.

  cd ..
  cabal sandbox init
  cabal sandbox add-source core/
  cabal install --only-dep
  cabal configure

You can then build the main project.

  cabal build
