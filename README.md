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
((:ID "ship01" :X1 0.125 :X2 0.1875 :Y1 0.67578125 :Y2 0.73828125)
 (:ID "ship02" :X1 0.10986328 :X2 0.23486328 :Y1 0.16894531 :Y2 0.25)
 (:ID "ship03" :X1 0.0 :X2 0.25 :Y1 0.0 :Y2 0.16894531)
 (:ID "ship04" :X1 0.0 :X2 0.12402344 :Y1 0.76123047 :Y2 0.8129883)
 (:ID "ship05" :X1 0.059570313 :X2 0.18457031 :Y1 0.45410156 :Y2 0.5283203)
 (:ID "ship06" :X1 0.0 :X2 0.125 :Y1 0.53027344 :Y2 0.62109375)
 (:ID "ship07" :X1 0.18457031 :X2 0.24707031 :Y1 0.4765625 :Y2 0.5136719)
 (:ID "ship08" :X1 0.0 :X2 0.122558594 :Y1 0.8129883 :Y2 0.88916016)
 (:ID "ship09" :X1 0.18457031 :X2 0.27246094 :Y1 0.39257813 :Y2 0.4765625)
 (:ID "ship10" :X1 0.23486328 :X2 0.30078125 :Y1 0.25195313 :Y2 0.3125)
 (:ID "ship11" :X1 0.125 :X2 0.22216797 :Y1 0.5283203 :Y2 0.59375)
 (:ID "ship12" :X1 0.0 :X2 0.12109375 :Y1 0.88916016 :Y2 0.97802734)
 (:ID "ship13" :X1 0.22216797 :X2 0.27978516 :Y1 0.5136719 :Y2 0.5703125)
 (:ID "ship14" :X1 0.18457031 :X2 0.27490234 :Y1 0.32910156 :Y2 0.39257813)
 (:ID "ship15" :X1 0.12402344 :X2 0.20751953 :Y1 0.76123047 :Y2 0.80908203)
 (:ID "ship16" :X1 0.17578125 :X2 0.2368164 :Y1 0.59375 :Y2 0.65478516)
 (:ID "ship17" :X1 0.23486328 :X2 0.28466797 :Y1 0.16894531 :Y2 0.25195313)
 (:ID "ship18" :X1 0.122558594 :X2 0.23583984 :Y1 0.8129883 :Y2 0.8833008)
 (:ID "ship19" :X1 0.12109375 :X2 0.234375 :Y1 0.88916016 :Y2 0.97802734)
 (:ID "ship20" :X1 0.1875 :X2 0.24121094 :Y1 0.65478516 :Y2 0.7114258)
 (:ID "ship21" :X1 0.0 :X2 0.10986328 :Y1 0.16894531 :Y2 0.36328125)
 (:ID "ship22" :X1 0.0 :X2 0.059570313 :Y1 0.36328125 :Y2 0.53027344)
 (:ID "ship23" :X1 0.125 :X2 0.17578125 :Y1 0.59375 :Y2 0.67578125)
 (:ID "ship24" :X1 0.0 :X2 0.125 :Y1 0.62109375 :Y2 0.69970703)
 (:ID "ship25" :X1 0.10986328 :X2 0.23486328 :Y1 0.25 :Y2 0.32910156)
 (:ID "ship26" :X1 0.10986328 :X2 0.17236328 :Y1 0.32910156 :Y2 0.36328125)
 (:ID "ship27" :X1 0.0 :X2 0.125 :Y1 0.69970703 :Y2 0.76123047)
 (:ID "ship28" :X1 0.234375 :X2 0.296875 :Y1 0.9477539 :Y2 0.9848633)
 (:ID "ship29" :X1 0.059570313 :X2 0.18457031 :Y1 0.36328125 :Y2 0.45410156)
 (:ID "ship30" :X1 0.234375 :X2 0.33447266 :Y1 0.8833008 :Y2 0.9477539))
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
