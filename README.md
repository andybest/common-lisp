# gamebox-sprite-packer

A spritesheet packer for games.

## Overview

This simple utility will pack images into a larger spritesheet, suitable for game development. It uses the 'maxrects' algorithm as described here: http://clb.demon.fi/files/RectangleBinPack.pdf

## Install

This system is not yet available to be installed automatically
with [Quicklisp](https://www.quicklisp.org). To manually install using Quicklisp, clone this
repository into your `local-projects` directory and issue the following in your REPL:

``` lisp
(ql:quickload :gamebox-sprite-packer)
```

## Usage

Create a spritesheet with the following:

``` lisp
(let ((rects (collect-rects #p"/path/to/image/files" :recursivep t)))
  (make-atlas rects :out-file #p"/path/to/spritesheet.png" :width 1024 :height 1024))
```

This will write an image to disk, as well as a metadata file of the same name with the extension ".sexp".

## License

Copyright © 2017 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
