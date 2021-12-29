# mfiano.misc.rng

Provides a convenient means of generating random numbers that are seedable with deterministic
results across hardware and Common Lisp implementations.

## Overview

This library is a light wrapper over cl-pcg, providing a convenient method of generating seedable
random numbers. Using it is as simple as creating a generator, and calling one of the various
generation functions on that generator.

Generators can be seeded with any string value, to aid in remembering the seed (rationale: words or
a sequence of words are easier to remember than numbers).

If a seed string is not provided when constructing a generator, a seed is randomly generated for
you. Such random seed strings are of a particular form to aid in remembering and sharing results
with others. To do this, the generator randomly chooses 5 words from a curated dictionary of 7776
words that are dissimilar from each other, and appends them together delimited by hyphen characters.

In addition to constructing a generator given a seed string or having one generated automatically,
we also support seeding a generator given another generator object. In this case, a seed string for
the new generator is randomly generated using the seed of the supplied generator. In this way, you
can have distinct nested generators giving independently deterministic results.

## Install

```lisp
(ql:quickload :mfiano.misc.rng)
```

## Usage

```lisp
(make-generator &optional source)
```

Construct a generator suitable for generating random numbers. The type of `source` determines how
the generator is seeded:

null: If `source` is NIL, a seed is randomly generated. This is useful if you don't care about
deterministic results.

string: Seeded using this string. Any generator with the same string seed will result in the same
sequence of random numbers.

generator: If given another generator as the source, a seed will be generated using the seed of the
generator supplied. In this way, you can have distinct nested generators giving independently
deterministic results.

```lisp
(get-seed generator)
```
Return the seed string of `generator`. In case an integer is needed, one is provided as a secondary
return value.

```lisp
(bool generator &optional (probability 0.5))
```
Randomly generate a boolean value, with `probability` chance of a true result.

```lisp
(int generator min max &optional (inclusive-p t))
```
Randomly generate an integer (fixnum) to be within the lower bound and upper bound denoted by `min`
and `max`. If `inclusive-p` is non-NIL (the default), then the range is inclusive.

```lisp
(float generator min max)
```
Randomly generate a single-precision floating point number to be within the lower bound and upper
bound denoted by `min` and `max`.

```lisp
(element generator sequence)
```
Randomly choose a single element from the given sequence.

```lisp
(shuffle generator sequence)
```
Randomly shuffle the given sequence, non-destructively.

```lisp
(die generator sides &key (modifier 0) (count 1))
```
Simulate rolling a die of `sides` sides `count` number of times, summing the results. `modifier`
is an additional value to sum with the final result.

## License

Copyright © 2021 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
