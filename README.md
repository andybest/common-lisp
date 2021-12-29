# mfiano.math.cubic-bezier

A library for constructing and evaluating cubic Bézier curve paths.

## Install

```lisp
(ql:quickload :mfiano.math.cubic-bezier)
```

## Usage

```lisp
(make-curve points &key (divisions 100))
```
Create a cubic Bézier curve path from the given sequence of `points`. `points` is a sequence of
3-dimensional vectors as constructed with `#'origin.vec3:vec`. `divisions` is the number of
sub-divisions to use when estimating the arc-length of the curve path: see the documentation for
`#'evaluate` for more information.

```lisp
(add-points curve points)
```
Append the specified `points` to `curve`. `points` should be a sequence of 3-dimensional vectors
as constructed with `#'origin.vec3:vec`. NOTE: It is an error to add less than a segment worth of
points. A cubic Bézier curve is defined by 4 points for the first segment, and three points for each
successive segment (since the first point of a segment is shared with the last point of the previous
segment.)

```lisp
(edit-point curve index value)
```
Edit the point of `curve` at index `index` with `value`. `value` should be a 3-dimensional vector
as constructed with `#'origin.vec3:vec`.

```lisp
(evaluate curve parameter &key even-spacing-p)
```
Evaluate `curve` at parameter `parameter`. If `even-spacing-p` is non-NIL, arc-length
re-parameterization is applied, which evenly spaces points along the curve. The number of points is
defined by the `divisions` argument supplied when constructing the curve with `#'make-curve`.
Arc-length re-parameterization is a remapping of `parameter` before evaluating the curve, in order to
allow for uses such as animation along a curve with a constant velocity.

```lisp
(point-count-valid-p point-count)
```
Check whether the integer `point-count` is a valid number of points for a cubic Bézier curve
path. To be valid, there must be at least 4 points, any increment of 3 thereafter: (4, 7, 10, 13,
...).

```lisp
(point-index-present-p curve index)
```
Check if `curve` has a point at index `index`.

```lisp
(collect-points curve count &key even-spacing-p)
```
Evaluate `count` points along curve, returning a list of 3-dimensional vectors. If
`even-spacing-p` is supplied, arc-length re-parameterization is applied: see the documentation for
`#'evaluate` for more information.

```lisp
(collect-segments curve count &key even-spacing-p)
```
Collect a list of `count` segments of `curve`. A segment is a list of two 3-dimensional vectors.
If `even-spacing-p` is supplied, arc-length re-parameterization is applied: see the documentation
for `#'evaluate` for more information.

## License

Copyright © 2021 Michael Fiano <mail@mfiano.net>.

Licensed under the MIT License.
