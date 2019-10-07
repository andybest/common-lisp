# patchwork

A spritesheet packer for games.

## Overview

This simple utility will pack images into a larger spritesheet, suitable for
game development. It uses the 'maxrects' algorithm as described
[here](http://clb.demon.fi/files/RectangleBinPack.pdf).

## Install

```lisp
(ql:quickload :patchwork)
```

## Usage

### Pack Sprites into a Spritesheet

A spritesheet is created from a collection of smaller files. To tell this
library which files you would like packed, you can do this one of three ways:

- Automatically scan for files in the current directory, and add them to a
spritesheet:

```lisp
(make-atlas-from-directory "/home/user/sprites"
                           :out-file #p"/tmp/spritesheet.png"
                           :width 1024
                           :height 1024)
```

- Automatically scan for files in the current directory recursively, and add
them to a spritesheet:

```lisp
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

```lisp
'((#p"/path/to/file1.png" . "bomb")
  (#p"/path/to/file2.png" . "missile"))
```

Once you have a `file-spec`, you can pass it to `MAKE-ATLAS` as follows:

```lisp
(make-atlas file-spec
            :out-file #p"/tmp/spritesheet.png"
            :width 1024
            :height 1024)
```

The above three methods will write an image to disk, as well as a metadata file
of the same name with the extension ".spec". The metadata file is a list of
property lists, specifying the ID's of the images and their positions and sizes
in the spritesheet. An example metadata file looks like the following:

```lisp
((:ID "ship01" :X 1316 :Y 1060 :W 140 :H 140)
 (:ID "ship02" :X 3770 :Y 1944 :W 268 :H 178)
 (:ID "ship03" :X 3502 :Y 1944 :W 268 :H 185)
 (:ID "ship04" :X 2823 :Y 3089 :W 266 :H 118)
 (:ID "ship05" :X 3234 :Y 2134 :W 268 :H 164)
 (:ID "ship06" :X 1751 :Y 3849 :W 268 :H 198)
 (:ID "ship07" :X 1584 :Y 932 :W 140 :H 88)
 (:ID "ship08" :X 3089 :Y 3129 :W 263 :H 168)
 (:ID "ship09" :X 3868 :Y 2886 :W 192 :H 184)
 (:ID "ship10" :X 3600 :Y 2453 :W 147 :H 136)
 (:ID "ship11" :X 3868 :Y 2598 :W 211 :H 146)
 (:ID "ship12" :X 2823 :Y 3207 :W 260 :H 194)
 (:ID "ship13" :X 1189 :Y 1577 :W 130 :H 128)
 (:ID "ship14" :X 3868 :Y 2744 :W 197 :H 142)
 (:ID "ship15" :X 1436 :Y 2113 :W 183 :H 110)
 (:ID "ship16" :X 3083 :Y 3297 :W 137 :H 137))
```

For the first two automatic generation methods above, IDs are automatically
generated based on the filename without the extension, and the path relative to
the root directory starting point. This disambiguates files of the same name
located in different directories.

You can also supply the `:normalize` argument to `make-atlas` or
`make-atlas-from-directory` to map pixels to the [0..1] domain when writing the
resulting metadata file. This is useful for use with OpenGL texture coordinates.

You can optionally supply the `:flip-y` argument to `make-atlas` or
`make-atlas-from-directory` to flip the Y axis when writing coordinates to the
metadata file. This is useful when using OpenGL which assumes the origin is at
the bottom-left.

Lastly, you can also supply the `:padding` argument to `make-atlas` or
`make-atlas-from-directory` to specify the amount of padding in pixels
separating each sprite in the atlas.

### Unpacking Sprites from a Spritesheet

You can perform the reverse operation, and reconstruct the original individual
sprite images given a spritesheet's image and metadata files.

```lisp
(unpack-atlas #p"/tmp/spritesheet.png"
              :out-path #p"/tmp/sprites/"
              :denormalize nil
              :flip-y nil)
```

This will unpack all the sprites in `/tmp/spritesheet.png` to the directory
`/tmp/sprites/`, assuming the metadata file `/tmp/spritesheet.spec` exists.

You can optionally supply `:denormalize t` if the metadata was created with
normalized floats rather than pixel integers.

You can also supply the `:flip-y t` option if the metadata was written with the
Y axis flipped.

## License

Copyright © 2017 [Michael Fiano](mailto:mail@michaelfiano.com).

Licensed under the MIT License.
