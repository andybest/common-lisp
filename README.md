# Origin

A high performance math library useful for making games.

## Overview

A Common Lisp game math library, providing support for common math functions
related to game development. Supported are:

* 2D vectors
* 3D vectors
* 4D vectors
* 2x2 matrices
* 3x3 matrices
* 4x4 matrices
* Quaternions

**Important**
This project is not conforming with the Common Lisp specification in that it
requires an implementation which supports "package-local nicknames". At the time
of writing, this currently includes: ABCL, SBCL, CCL, ECL, and Clasp.

## Install

``` lisp
(ql:quickload :origin)
```

## License

Copyright © 2014-2018 [Michael Fiano](mailto:mail@michaelfiano.com).

Licensed under the MIT License.
