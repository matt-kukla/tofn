# tofn
Typed ordered fuzzy numbers and arithmetic operations, implemented
in OCaml.

## Introduction
Arithmetic operations on fuzzy numbers are traditionally carried out via 
interval-based methods.  However, these operations are prone to 
accumulating uncertainty, which presents issues in situations where many operations are performed in succession (e.g fuzzy autoregressive models, 
aggregations).

Ordered fuzzy numbers (OFNs) remedy this issue by augmenting fuzzy numbers
with a notion of *orientation* or *trend*.  This gives
arithmetic operations with unique additive inverses, allowing 
accumulated uncertainty to be reversed.  Orientation has also proven 
useful for modeling [temporal data](https://arxiv.org/abs/2011.01980).

Typed ordered fuzzy numbers (TOFNs) are OFNS constructed from functions of a common family or *type*.  Arithmetic operations between TOFNs are 
intended to preserve both the type and (fuzzy) set-theoretic
properties of their operands.

The general theory and construction of TOFNs, along with a discussion of
imprecision control, may be found in
*[Rings of Typed Ordered Fuzzy Numbers](https://arxiv.org/abs/2010.07764)*.
This library implements several common families of TOFNs and
associated arithemetic operations.

## Installation
To build/install the contents of this repository locally, run 
`dune build`, followed by `dune install`.

This package may also be installed from opam with `opam install tofn`.

## Documentation
Documentation can be built with ```ocamldoc```, or accessed [online](http://mkukla.net/doc/tofn/Tofn.html).

## Author
* [Matt Kukla](https://matt-kukla.github.io) (<matt.kukla@yandex.com>)

## License
This project is licensed under the GNU GPL v3 - see [LICENSE](LICENSE)
for details.
