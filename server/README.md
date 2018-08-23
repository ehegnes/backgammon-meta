backgammon-meta's server
========================

## Makefile Targets
| Target           | Description                                                                                           |
| ---------------- | ----------------------------------------------------------------------------------------------------- |
| `all`            | `build` and `test`                                                                                    |
| `build`          | Generate [backgammonlogic.h](backgammon-logic-wrapper/include/backgammonlogic.h) and build the server |
| `test`           | Test marshalling data between Rust and Haskell                                                        |
| `run`            | Start the server                                                                                      |
| `generate-types` | Generate `elm` types for use in the client                                                            |
| `clean`          | `stack clean` and `cargo clean`     `                                                                 |
| `todo`           | `grep` for certain comment predicates that indicate work to be done                                   |
