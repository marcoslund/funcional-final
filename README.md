# funcional-final
Final Programación Funcional

## Setup
First, install Haskell. The easiest way is with `ghcup` on Unix-like systems and with `Chocolatey` on Windows. The Haskell version used here is `8.8.4`.
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

## Endpoints
```
GET /
```

### Admin namespace
These endpoints start with `/admin`

```
POST /products
GET /products?page=xxx&name=yyy
GET /products/<id>
PUT /products/<id>
DELETE /product/<id>
```
