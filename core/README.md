## How to build?

Create and configured a sandbox for core project.

```
cabal sandbox init
cabal install --enable-tests --only-dep
cabal configure --enable-tests
```

Then you can run tests to ensure that the code works as expected.

```
cabal test
```
