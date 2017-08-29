[![Quicklisp](http://quickdocs.org/badge/gamebox-math.svg)](http://quickdocs.org/gamebox-math/)
[![Build Status](https://travis-ci.org/mfiano/gamebox-math.svg?branch=master)](https://travis-ci.org/mfiano/gamebox-math)

# gamebox-math

A high performance math library useful for making games.

## Overview

A library written in portable Common Lisp, providing support for common math
functions related to game development. Supported are 3-component vectors, 4x4
matrices, quaternions, and other high-level functions one may need when writing
a game.

## Install

``` lisp
(ql:quickload :gamebox-math)
```

## Usage

This library should be fairly self-explanatory to anyone familiar with linear
algebra, and each symbol has a documentation string containing more detail.

The library follows a certain naming convention.
Functions having to do with vectors, points, lines, quaternions, dual quaternions,
and matrices are prefixed, respectively, with `vec-`, `point-`, `line-`, `quat-`,
`dquat-`, and `matrix-`. Some functions do not include the dash, notably addition,
subtraction, multiplication, and comparison functions. Each function has two variants,
one destructive and one consing. The destructing ones use `!` as a postfix.

Constructor functions for each of the above data types have the names listed above,
also without the dash. For example, `(vec)` produces a fresh vector.

Each of the above data types also have several ways of accessing their members.
Vecs, quats, and matrices can be accessed by index with the functions `vref`, `qref`,
and `mref`, respectivly. Macros for simple access is available under the names `with-X`
where `X` is `vec`, `quat`, `matrix`, or their plural forms. Example use:
```lisp
(with-vec (v my-vector)
  ; symbols vx, vy, and vz now available as accessors
)
(with-vecs ((v1 foo-vector)
            (v2 bar-vector)
            ...)
  ; symbol accessors for any number of vectors can now be used
)
```
Quats use x, y, z, w; matrices use 00 to 33; Dquats are like quats except with r and d
added to the front of the member names, for the real and the dual part respectively.

## License

Copyright © 2014 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
