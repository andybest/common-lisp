# mfiano.data-structures.identifier-pool

A simple generational identification number allocator.

## Overview

This library is able to generate increasing identifiers (integers) in such a way that previously
deallocated identifiers are available to be reclaimed by the generator the next time one is
allocated. Essentially, it solves the "ABA Problem": https://en.wikipedia.org/wiki/ABA_problem

It does so in a space-efficient manner, without the need for the storage of a list of deallocated
identifiers. It does this by keeping deallocated identifiers around and modifying their data on
deletion to build a sort of implicit linked list. That is, when an identifier is marked for
deletion, the following occurs:

* The packed version portion of its data is incremented.

* The packed ID portion of its data is set to the integer stored in the pool's FREE-HEAD slot
  (FREE-HEAD can be thought of as the head of the implicit linked list). If FREE-HEAD is null, then
  instead, all of the bits of the ID are set. #xFFFFFF represents the "invalid" ID.

* FREE-HEAD is set to the ID portion of its data.

This, in effect, constructs an implicit linked list of the next available identifiers that can be
generated.

## Install

```lisp
(ql:quickload :mfiano.data-structures.identifier-pool)
```

## License

Copyright © 2019-2021 Michael Fiano <mail@mfiano.net>

Licensed under the MIT License.
