# flac-metadata

A utility for reading metadata embedded in FLAC audio files.

## Overview

This utility was created as an alternative to
[flac-parser](https://github.com/mfiano/flac-parser). They do the same thing
currently, as both projects parse FLAC metadata. However, `flac-metadata` goes
about it a much different way, which results in much more concise code, and
eventually, it should be faster. Eventually, because one of its dependencies,
`bitio`, needs some additional performance improvements when reading sequences
of bytes, which the author is aware of and working towards a solution. In
addition, `flac-metadata` and `flac-parser` will separate paths when/if
`flac-parser` gains support for decoding audio frames. In short, I wrote
`flac-metadata` to see how concise I could do it, and to try out a different
parsing method that I used to write
[pngload](https://github.com/mfiano/pngload).

## Install

This system is not yet available to be installed automatically with Quicklisp.
To manually install using Quicklisp, clone this repository into your
local-projects directory and issue the following in your REPL:

```lisp
(ql:quickload :flac-metadata)
```

## Usage

You can read metadata from a FLAC audio file in one of two ways.

The following will return an object, representing the concrete syntax tree of
the FLAC specification. With it, you can access the metadata you wish:

```lisp
(load-file #p"/path/to/file.flac")
```

Alternatively, you may dump out the metadata pretty-printed:

```lisp
(dump-file (load-file #p"/path/to/file.flac"))
```

## License

Copyright © 2017 Michael Fiano <mail@michaelfiano.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
