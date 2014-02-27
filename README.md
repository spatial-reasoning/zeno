Zeno - The Library for Qualitative Spatio-Temporal Reasoning
============================================================

Zeno is a library for qualitative spatio-temporal reasoning written in
Haskell. It also contains a suite of benchmarks for qualitative spatio-temporal
calculi.

It provides several semi-decision procedures for the consistency of constraint
networks in qualitative spatio-temporal calculi. In addition to native
implementations it interfaces the common reasoning tools »SparQ«, »Gqr»,
»lp\_solve« and »Yices«.

It also contains a testsuite of benchmarks and an automated benchmarking tool
comparing the given decision procedures regarding several parameters of
interest like the density of atomic networks and the common phase transition of
the tested procedures. For evaluation the results are presented in a nice
graphical output.

## Procedures implemented or integrated so far are:

- __Algebraic Closure__ (binary and ternary) (via GQR and SparQ)
- __Algebraic Geometric Reasoning__ (via SparQ)
- __Triangle Consistency for the LR Calculus__ (cf Lücke et al.)
- __Oriented Matroids__ (including Biquadratic Final Polynomials)
- __Angle Consistency for Oriented Points (and thus for OPRA-m)__

There will be a manual soon.

## Collaboration welcome!

We strongly believe in the power of collaboration instead of competition. So if
you have any wishes or ideas please let us know.

## Discuss

- Questions and Discussion: A mailing list might be available sometime. So long
  please feel free to
  ask [André van Delden](mailto:andre.van.delden@uni-bremen.de) personally.
- Bug reports: [Issue tracker](https://github.com/weltensegler/zeno/issues)

## Authors

- [André Scholz] (http://www.math.uni-bremen.de/~schola/)

## License

Zeno is distributed under a BSD License with one important addition. See
[LICENSE](https://github.com/weltensegler/zeno/blob/master/LICENSE) for
details.

