# gamebox-sprite-packer

A spritesheet packer for games.

## Overview

This simple utility will pack images into a larger spritesheet, suitable for
game development. It uses the 'maxrects' algorithm as described
[here](http://clb.demon.fi/files/RectangleBinPack.pdf).

## Install

``` lisp
(ql:quickload :gamebox-sprite-packer)
```

## Usage

A spritesheet is created from a collection of smaller files. To tell this
library which files you would like packed, you can do this one of three ways:

- Automatically scan for files in the current directory, and add them to a
  spritesheet:

``` lisp
(make-atlas-from-directory "/home/user/sprites"
                           :out-file #p"/tmp/spritesheet.png"
                           :width 1024
                           :height 1024)
```

- Automatically scan for files in the current directory recursively, and add
  them to a spritesheet:

``` lisp
(make-atlas-from-directory "/home/user/sprites"
                           :recursive t
                           :out-file #p"/tmp/spritesheet.png"
                           :width 1024
                           :height 1024)
```

- Manually specify the files to be added to a spritesheet:

To manually add files, you must construct a `file-spec`, which is an association
list mapping absolute pathnames of files to their unique ID. The unique ID is
what will be written to the resulting metadata file. As an example, you can
create a list such as:

``` lisp
'((#p"/path/to/file1.png" . "bomb")
  (#p"/path/to/file2.png" . "missile"))
```

Once you have a `file-spec`, you can pass it to `MAKE-ATLAS` as follows:

``` lisp
(make-atlas file-spec
            :out-file #p"/tmp/spritesheet.png"
            :width 1024
            :height 1024)
```

The above three methods will write an image to disk, as well as a metadata file
of the same name with the extension ".spec". The metadata file is a list of
property lists, specifying the ID's of the images and their positions and sizes
in the spritesheet. An example metadata file looks like the following:

``` lisp
((:ID "ship000" :X1 41 :X2 51 :Y1 317 :Y2 324 :W 10 :H 7)
 (:ID "ship001" :X1 94 :X2 112 :Y1 240 :Y2 252 :W 18 :H 12)
 (:ID "ship002" :X1 289 :X2 324 :Y1 234 :Y2 258 :W 35 :H 24)
 (:ID "ship003" :X1 234 :X2 289 :Y1 199 :Y2 237 :W 55 :H 38)
 (:ID "ship004" :X1 94 :X2 174 :Y1 88 :Y2 143 :W 80 :H 55)
 (:ID "ship005" :X1 51 :X2 61 :Y1 317 :Y2 324 :W 10 :H 7)
 (:ID "ship006" :X1 154 :X2 174 :Y1 198 :Y2 212 :W 20 :H 14)
 (:ID "ship007" :X1 141 :X2 181 :Y1 296 :Y2 324 :W 40 :H 28)
 (:ID "ship008" :X1 94 :X2 154 :Y1 198 :Y2 240 :W 60 :H 42)
 (:ID "ship009" :X1 94 :X2 174 :Y1 143 :Y2 198 :W 80 :H 55)
 (:ID "ship010" :X1 182 :X2 250 :Y1 78 :Y2 158 :W 68 :H 80)
 (:ID "ship011" :X1 315 :X2 324 :Y1 61 :Y2 71 :W 9 :H 10)
 (:ID "ship012" :X1 209 :X2 227 :Y1 235 :Y2 256 :W 18 :H 21)
 (:ID "ship013" :X1 289 :X2 319 :Y1 199 :Y2 234 :W 30 :H 35)
 (:ID "ship014" :X1 270 :X2 315 :Y1 0 :Y2 53 :W 45 :H 53)
 (:ID "ship015" :X1 182 :X2 270 :Y1 0 :Y2 78 :W 88 :H 78)
 (:ID "ship016" :X1 313 :X2 324 :Y1 143 :Y2 153 :W 11 :H 10)
 (:ID "ship017" :X1 0 :X2 21 :Y1 304 :Y2 323 :W 21 :H 19)
 (:ID "ship018" :X1 141 :X2 186 :Y1 256 :Y2 296 :W 45 :H 40)
 (:ID "ship019" :X1 250 :X2 313 :Y1 143 :Y2 199 :W 63 :H 56)
 (:ID "ship020" :X1 0 :X2 94 :Y1 188 :Y2 252 :W 94 :H 64)
 (:ID "ship021" :X1 312 :X2 324 :Y1 53 :Y2 61 :W 12 :H 8)
 (:ID "ship022" :X1 209 :X2 234 :Y1 218 :Y2 235 :W 25 :H 17)
 (:ID "ship023" :X1 154 :X2 209 :Y1 218 :Y2 256 :W 55 :H 38)
 (:ID "ship024" :X1 0 :X2 76 :Y1 252 :Y2 304 :W 76 :H 52)
 (:ID "ship025" :X1 313 :X2 323 :Y1 153 :Y2 163 :W 10 :H 10)
 (:ID "ship026" :X1 270 :X2 291 :Y1 53 :Y2 74 :W 21 :H 21)
 (:ID "ship027" :X1 186 :X2 228 :Y1 256 :Y2 298 :W 42 :H 42)
 (:ID "ship028" :X1 76 :X2 141 :Y1 252 :Y2 317 :W 65 :H 65)
 (:ID "ship029" :X1 0 :X2 94 :Y1 0 :Y2 94 :W 94 :H 94)
 (:ID "ship030" :X1 313 :X2 323 :Y1 163 :Y2 173 :W 10 :H 10)
 (:ID "ship031" :X1 291 :X2 312 :Y1 53 :Y2 74 :W 21 :H 21)
 (:ID "ship032" :X1 228 :X2 270 :Y1 237 :Y2 279 :W 42 :H 42)
 (:ID "ship033" :X1 250 :X2 315 :Y1 78 :Y2 143 :W 65 :H 65)
 (:ID "ship034" :X1 0 :X2 94 :Y1 94 :Y2 188 :W 94 :H 94)
 (:ID "ship035" :X1 313 :X2 323 :Y1 173 :Y2 183 :W 10 :H 10)
 (:ID "ship036" :X1 21 :X2 41 :Y1 304 :Y2 324 :W 20 :H 20)
 (:ID "ship037" :X1 228 :X2 268 :Y1 279 :Y2 319 :W 40 :H 40)
 (:ID "ship038" :X1 174 :X2 234 :Y1 158 :Y2 218 :W 60 :H 60)
 (:ID "ship039" :X1 94 :X2 182 :Y1 0 :Y2 88 :W 88 :H 88))
```

For the first two automatic generation methods above, IDs are automatically
generated based on the filename without the extension, and the path relative to
the root directory starting point. This disambiguates files of the same name
located in different directories.

You can also supply the `:normalize` argument to `make-atlas` or
`make-atlas-from-directory` to map pixels to the [0..1] domain when writing the
resulting metadata file. This is useful for use with OpenGL texture coordinates.

## License

Copyright © 2017 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
