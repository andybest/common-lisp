# mfiano.data-structures.dynamic-array

An implementation of the AVL tree data structure.

An optimized 1-dimensional array of fixnums that automatically re-adjusts in size.

## Overview

An optimized 1-dimensional array type that can only store fixnums, and which is able to
automatically grow when pushing new elements to the end. It is not very useful in most situations,
as it doesn't implement the Common Lisp sequences protocol, nor does it conform to the usual
interface of Common Lisp arrays for PUSH and POP.

## Install

```lisp
(ql:quickload :mfiano.data-structures.dynamic-array)
```

## License

Copyright © 2019-2021 Michael Fiano <mail@mfiano.net>

Licensed under the MIT License.
