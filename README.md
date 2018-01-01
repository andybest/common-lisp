[![Quicklisp](http://quickdocs.org/badge/gamebox-math.svg)](http://quickdocs.org/gamebox-math/)
[![Build Status](https://travis-ci.org/mfiano/gamebox-math.svg?branch=master)](https://travis-ci.org/mfiano/gamebox-math)

# gamebox-math

A high performance math library useful for making games.

## Overview

A library written in portable Common Lisp, providing support for common math
functions related to game development. Supported are 2D, 3D, and 4D vectors, 4x4
matrices, quaternions, and dual quaternions.

## Install

``` lisp
(ql:quickload :gamebox-math)
```

## Usage

This library should be fairly self-explanatory to anyone familiar with linear
algebra and each symbol has a documentation string containing more detail.

The library follows a certain naming convention. Functions having to do with
vectors, matrices, quaternions, dual quaternions are prefixed, respectively,
with `vecN-`, `mat4`, `quat`, and `dquat`. Each function has two variants, one
destructive and one consing. The destructive ones use `!` as a postfix.

## License

Copyright © 2014-2018 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
