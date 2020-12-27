# flac-metadata

A utility for reading metadata embedded in FLAC audio files.

## Overview

This utility was created as an alternative to
[flac-parser](https://github.com/mfiano/flac-parser). They both do the same
thing, but flac-metadata is faster, more concise, and I decided to abandon
flac-parser.

## Install

```lisp
(ql:quickload :flac-metadata)
```

## Usage

You can read metadata from a FLAC audio file in one of two ways.

The following will return an object, representing the concrete syntax tree of
the FLAC specification. With it, you can access the metadata you wish:

```lisp
(load-file #p"/path/to/file.flac") ; => #<FLAC-METADATA::FLAC {100490C5F3}>
```

Alternatively, you may dump out the metadata pretty-printed:

```lisp
(dump-file #p"/path/to/file.flac")

#|

File                                    /path/to/file.flac
  Marker                                fLaC
  Metadata Blocks
    Stream Information                  34 bytes
      Minimum block size                4,096 bytes
      Maximum block size                4,096 bytes
      Minimum frame size                14 bytes
      Maximum frame size                12,297 bytes
      Sample rate                       44,100Hz
      Channels                          1
      Bit depth                         16 bits per sample
      Total samples                     5,157,936
      MD5 checksum                      A2331F9298112C0E2121A371898B08D4
    Seek Table                          216 bytes
      Seek Point
        Target sample                   0
        Offset                          0
        Sample count                    4,096
      Seek Point
        Target sample                   438,272
        Offset                          1,068,557
        Sample count                    4,096
      Seek Point
        Target sample                   880,640
        Offset                          2,185,151
        Sample count                    4,096
      Seek Point
        Target sample                   1,318,912
        Offset                          3,327,723
        Sample count                    4,096
      Seek Point
        Target sample                   1,761,280
        Offset                          4,511,460
        Sample count                    4,096
      Seek Point
        Target sample                   2,203,648
        Offset                          5,723,533
        Sample count                    4,096
      Seek Point
        Target sample                   2,641,920
        Offset                          6,845,851
        Sample count                    4,096
      Seek Point
        Target sample                   3,084,288
        Offset                          7,985,828
        Sample count                    4,096
      Seek Point
        Target sample                   3,526,656
        Offset                          9,136,439
        Sample count                    4,096
      Seek Point
        Target sample                   3,964,928
        Offset                          10,295,520
        Sample count                    4,096
      Seek Point
        Target sample                   4,407,296
        Offset                          11,529,075
        Sample count                    4,096
      Seek Point
        Target sample                   4,849,664
        Offset                          12,563,214
        Sample count                    4,096
    Vorbis Comment                      1,405 bytes
      Vendor                            reference libFLAC 1.3.1 20141125
      Comments count                    53
        ACOUSTID_FINGERPRINT            <empty>
        ACOUSTID_ID                     <empty>
        ALBUM                           Breath of Fire Original Soundtrack Special Box
        ALBUM ARTIST                    Various Artists
        ALBUMARTIST                     Various Artists
        ALBUMARTISTSORT                 Various Artists
        ALBUMARTIST_CREDIT              Various Artists
        ARTIST                          Yuko Takehara
        ARTISTSORT                      Yuko Takehara
        ARTIST_CREDIT                   Yuko Takehara
        ASIN                            <empty>
        BPM                             0
        CATALOGNUMBER                   CPCA-10146
        COMPILATION                     1
        COMPOSER                        <empty>
        DATE                            2006
        DISC                            3
        DISCC                           11
        DISCNUMBER                      3
        DISCSUBTITLE                    <empty>
        DISCTOTAL                       11
        ENCODEDBY                       <empty>
        ENCODER                         <empty>
        GENRE                           Electronic
        GROUPING                        <empty>
        LABEL                           Suleputer
        LANGUAGE                        eng
        LYRICS                          <empty>
        MEDIA                           CD
        MUSICBRAINZ_ALBUMARTISTID       89ad4ac3-39f7-470e-963a-56509c546377
        MUSICBRAINZ_ALBUMCOMMENT        <empty>
        MUSICBRAINZ_ALBUMID             46ac4441-e568-4a37-8fc2-22368b3a6dd9
        MUSICBRAINZ_ALBUMSTATUS         Pseudo-Release
        MUSICBRAINZ_ALBUMTYPE           soundtrack
        MUSICBRAINZ_ARTISTID            7a463c32-b9b3-4143-8c4c-96590473dad0
        MUSICBRAINZ_RELEASEGROUPID      4085f1c7-f74a-4c7d-bd4a-cc148b65d115
        MUSICBRAINZ_TRACKID             f9eea541-873f-422b-b28e-385f069cdff7
        ORIGINALDATE                    2006-03-31
        PUBLISHER                       Suleputer
        RELEASECOUNTRY                  JP
        REPLAYGAIN_ALBUM_GAIN           -6.12 dB
        REPLAYGAIN_ALBUM_PEAK           1.000031
        REPLAYGAIN_TRACK_GAIN           -7.21 dB
        REPLAYGAIN_TRACK_PEAK           0.858699
        SCRIPT                          Latn
        TITLE                           We're Rangers
        TOTALDISCS                      11
        TOTALTRACKS                     23
        TRACK                           9
        TRACKC                          23
        TRACKNUMBER                     9
        TRACKTOTAL                      23
        YEAR                            2006
    Picture                             58243
        Type                            0
        MIME type                       image/jpeg
        Description
        Width                           0px
        Height                          0px
        Color depth                     0bpp
        Indexed colors                  0
        Size                            58,201
  Frames                                <not implemented>

|#
```

## License

Copyright © 2017-2020 [Michael Fiano] <mail@mfiano.net>.

Licensed under the MIT License.
