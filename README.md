Backgammon
==========

This aims to be a mostly-functional implemnetation of backgammon using Rust and
Haskell, complete with a web client. Much of the logic is adapted from
[gnubg](http://www.gnubg.org/).

## Logic & Design
### Board Representation
The board is a 2-by-25-index list of integers, corresponding to the bar (index 0)
and the points (indicies 1 to 24, inclusive). The 2-index dimensionality
corresponds to each respective player.

### API & Server
- Use JSON serialization to normalize data
- Server checks moves as they are attempted and reports to the appropriate player
- Server ensures synchronized boards between clients

## Resources
- [Backgammon Glossary](http://www.bkgm.com/glossary.html)
