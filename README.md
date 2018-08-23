backgammon-meta
===============

This project aims to provide a web-based backgammon game. The server is to be
in [Haskell](https://www.haskell.org/). The client is to use
[Elm](http://elm-lang.org/). All logic is to be contained in a
[Rust](https://www.rust-lang.org/) library and used via Foreign Function
Interfaces.

## Pieces Parts
- [Client](client)
- [Server](server)
- [Backgammon Logic](https://github.com/ehegnes/backgammon-logic)
- [Backgammon Logic Wrapper](server/backgammon-logic-wrapper)

## Resources
- [Backgammon Glossary](http://www.bkgm.com/glossary.html)
- [`gnubg`](https://savannah.gnu.org/cvs/?group=gnubg)
- [`mmakowski/backgammon-model`](https://github.com/mmakowski/backgammon-model)
