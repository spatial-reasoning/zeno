QSTRLib - The Library for Qualitative Spatio-Temporal Reasoning
===============================================================

QSTRLib is a library for qualitative spatio-temporal reasoning written in
Haskell. It also contains a suite of benchmarks for qualitative spatio-temporal
calculi.

It provides several semi-decision procedures for the consistency of constraint
networks in qualitative spatio-temporal calculi. In addition to native
implementations it interfaces the common reasoning tools »SparQ«, »Gqr»,
»lp\_solve« and »Yices«.

It also contains a testsuite of benchmarks and an automated benchmarking tool
comparing the given decision procedures regarding several parameters of
interest like the density of atomic networks and the common phase transition of
the tested procedures.

## Procedures implemented so far are:

- __Algebraic Closure__ (binary and ternary) (via SparQ and Gqr)
- __Gröbner Reasoning__ (via SparQ)
- __Triangle Consistency__ (cf Lücke et al.)
- __Oriented Matroids__ (sloppy and exact)
- __Biquadratic Final Polynomials__

There will be a manual some time soon.

## Collaboration welcome!

We strongly believe in the power of collaboration instead of competition. So if
you have any wishes or ideas please let us know.

## Discuss

- Questions and Discussion: There is a (very vacant) IRC channel #qstrlib on
  freenode. A mailing list will be available soon. So long please feel free to
  ask [André Scholz](mailto:andre.scholz@uni-bremen.de) personally.
- Bug reports: [Issue tracker](https://github.com/weltensegler/qstrlib/issues)

## Authors

- [André Scholz] (http://www.math.uni-bremen.de/~schola/)
- [Till Mossakowski] (http://www.math.uni-bremen.de/~till/)

## License

QSTRLib is distributed under a BSD License with one important addition. See
[LICENSE](https://github.com/weltensegler/qstrlib/blob/master/LICENSE) for
details.

