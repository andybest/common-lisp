# gamebox-sprite-packer

A spritesheet packer for games.

## Overview

This simple utility will pack images into a larger spritesheet, suitable for game development. It uses the 'maxrects' algorithm as described [here](http://clb.demon.fi/files/RectangleBinPack.pdf).

## Install

This system is not yet available to be installed automatically
with [Quicklisp](https://www.quicklisp.org). To manually install using Quicklisp, clone this
repository into your `local-projects` directory and issue the following in your REPL:

``` lisp
(ql:quickload :gamebox-sprite-packer)
```

## Usage

A spritesheet is created from a collection of smaller files. To tell this library which files you would like packed, you can do this one of three ways:

- Automatically scan for files in the current directory, and add them to a spritesheet:

``` lisp
(make-atlas-from-directory #p"/home/user/sprites"
                           :out-file #p"/tmp/spritesheet.png"
                           :width 1024
                           :height 1024)
```

- Automatically scan for files in the current directory recursively, and add them to a spritesheet:

``` lisp
(make-atlas-from-directory #p"/home/user/sprites"
                           :recursivep t
                           :out-file #p"/tmp/spritesheet.png"
                           :width 1024
                           :height 1024)
```

- Manually specify the files to be added to a spritesheet:

To manually add files, you must construct a `file-spec`, which is an association list mapping absolute pathnames of files to their unique ID. The unique ID is what will be written to the resulting metadata file. As an example, you can create a list such as:

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

The above three methods will write an image to disk, as well as a metadata file of the same name with the extension ".sexp". The metadata file is a list of property lists, specifying the ID's of the images and their positions and sizes in the spritesheet. An example metadata file looks like the following:

``` lisp
((:ID "ship01" :X 814 :Y 240 :W 128 :H 128)
 (:ID "ship02" :X 225 :Y 346 :W 256 :H 166)
 (:ID "ship03" :X 0 :Y 0 :W 512 :H 346)
 (:ID "ship04" :X 737 :Y 0 :W 254 :H 106)
 (:ID "ship05" :X 481 :Y 346 :W 256 :H 152)
 (:ID "ship06" :X 122 :Y 744 :W 256 :H 186)
 (:ID "ship07" :X 0 :Y 1116 :W 128 :H 76)
 (:ID "ship08" :X 512 :Y 106 :W 251 :H 156))
```

For the first two automatic generation methods above, IDs are automatically generated based on the filename without the extension, and the path relative to the root directory starting point. This disambiguates files of the same name located in different directories.

## License

Copyright © 2017 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
