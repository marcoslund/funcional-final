# funcional-final
Final Programación Funcional

## Setup
First, install Haskell. The easiest way is with [ghcup](https://www.haskell.org/ghcup/) on Unix-like systems and with [Chocolatey](https://chocolatey.org/install) on Windows. The Haskell version used here is `8.8.4`.
After installing everything and cloning the project open a terminal and run:
```bash
cabal update
cabal repl
```
Dependencies should begin to install. Maybe some dependencies are not fetched properly and it may be necessary to run:
```bash
cabal v1-install <missing-dependency>
```
Finally, when entering `ghci` after `cabal repl` run:
```haskell
main
```
