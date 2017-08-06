# gamebox-sprite-packer

A spritesheet packer for games.

## Overview

This simple utility will pack images into a larger spritesheet, suitable for game development.

## Install

This system is not yet available to be installed automatically
with [Quicklisp](https://www.quicklisp.org). To manually install using Quicklisp, clone this
repository into your `local-projects` directory and issue the following in your REPL:

``` lisp
(ql:quickload :gamebox-sprite-packer)
```

## Usage

``` lisp
(let ((rects (collect-rects #p"/path/to/sprites" :recursivep t)))
  (make-atlas rects :out-file #p"/path/to/output" :width 1024 :height 1024))
```

## License

Copyright © 2017 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
