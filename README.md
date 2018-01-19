# shadow

A lightweight system to help with defining and managing OpenGL shader programs.

## Overview

Under the hood, Shadow is just a wrapper around the [Varjo](https://github.com/cbaggers/varjo)
library used for writing shader programs, and some fluff to allow referencing shader programs by
name, querying for basic information about them, as well as modifying uniform variables throughout
the lifecycle of an OpenGL application. Limited and experimental support for writing to uniform
buffer objects is also available.

The goal of Shadow is to be a simple solution to ease the task of writing and managing OpenGL shader
programs, and as such, does not and will not contain every feature you might need.

## Install

``` lisp
(ql:quickload :shadow)
```

## Usage

Using Shadow is not very straightforward, mostly due to the borrowing of the "Vari" language used to
write shader programs, which does not have much documentation. It does however try to stay familiar
and resembles Common Lisp. Additionally, there are [several
videos](https://www.youtube.com/watch?v=82o5NeyZtvw&list=PL2VAYZE_4wRITJBv6saaKouj4sWSG1FcS) of its
usage created by its author.

Shader programs are written using a series of `DEFUN-GPU` and `DEFSTRUCT-GPU` forms representing GPU
functions and structures respectively. As mentioned, their bodies follow the language rules of
"Vari", which is not documented here.

Each `DEFUN-GPU` defines a shader stage or auxillary function thereof. It takes in input arguments
and uniform variables, and sends its return values to the next stage of the shader pipeline as input
arguments. The vertex stage's input arguments correspond to your Vertex Array Object attributes.

A simple OpenGL shader program:

```lisp
(defun-gpu foo-vert ((position :vec3) (uv :vec2) &uniform (mvp :mat4))
  (values (* mvp (vec4 position 1))
          (vec4 1 0 0 1)))

(defun-gpu foo-frag ((color :vec4))
  (values color))
```

This defines 2 GPU functions, `foo-vert` and `foo-frag` that will serve as a very simple program
once translated and compiled.

To use this program it first must be translated from the Lisp-like "Vari" language, into GLSL. This
is done with the `MAKE-PROGRAM` macro:

```lisp
(make-program :example-program (:version 330 :primitive :points)
  (:vertex () (foo-vert :vec3 :vec2))
  (:fragment () (foo-frag :vec4)))
```

Above, we call `MAKE-PROGRAM` with a name to call our program, `:example-program`, the default
stage version to use, `:version 330`, and the OpenGL drawing primitive the vertex stage should use,
`:primitive :points`, followed by a sequence of "stage-specs" of the form: `(stage-type (version)
function-spec)`:

`stage-type` may be one of: `:vertex`, `:tessellation-control`, `:tessellation-evaluation`,
`:geometry`, `:fragment`, or `:compute`.

`version` is a number, string, or symbol representing a GLSL version to use when compiling this
stage, for example `330`, `"330"`, `:330`, or `'|330|` are all equivalent. The default version if
not specified is derived from the options list of the containing `MAKE-PROGRAM` form.

`func-spec` specifies which `DEFUN-GPU` function to use for this stage, and is a list consisting of
the function name followed by the types of all of its input arguments. The types are important
because the "Vari" shader language allows the same function name to exist with different signatures,
so you must be explicit in which function you want to translate to GLSL.

Issuing the call to `MAKE-PROGRAM` produces a `PROGRAM` object, which includes some useful
information:

The `SOURCE` reader method is a hash table with keys consisting of shader stage keyword symbols, and
values being strings of the translated "Vari" to GLSL source code:

```lisp
(make-program ...)

(source *) ; => #<HASH-TABLE :TEST EQL :COUNT 2 {10020C7603}>

(gethash :vertex *)
#|
"#version 330

layout(location = 0)  in vec3 POSITION;
layout(location = 1)  in vec2 UV;

out _FROM_VERTEX_STAGE_
{
     out vec4 _VERTEX_STAGE_OUT_1;
} v_out;

uniform mat4 MVP;

void main()
{
    gl_Position = (MVP * vec4(POSITION,float(1)));
    v_out._VERTEX_STAGE_OUT_1 = vec4(float(1),float(0),float(0),float(1));
    return;
}"
T
|#

(gethash :fragment **)
#|
"#version 330

in _FROM_VERTEX_STAGE_
{
     in vec4 _VERTEX_STAGE_OUT_1;
} v_in;

layout(location = 0)  out vec4 _FRAGMENT_STAGE_OUT_0;

void main()
{
    _FRAGMENT_STAGE_OUT_0 = v_in._VERTEX_STAGE_OUT_1;
    return;
}"
T
|#
```

As can be seen by the GLSL source, our vertex stage function is properly making use of the `VALUES`
form. It takes the first value for itself, setting `gl_Position`, and passes all subsequent values
as input arguments to the fragment stage, `(vec4 1 0 0 1)`, which takes that for itself as the final
fragment color of the pipeline.

So far, we have only translated the "Vari" shader language into the GLSL language understood by
GPUs. We still have to compile the shader stages and link the final program object on the GPU.

At this point, a valid OpenGL context is needed to continue.

To compile a program's stages and link them into a program, you can use the `BUILD-PROGRAM`
function:

```lisp
(build-program :example-program)
```

This will compile all of the stages previously translated to GLSL in our `:example-game` program,
and link it into a program object on the GPU. This returns a non-zero integer on success.

Alternatively, you can compile and link all GLSL translated programs in one shot, by using the
`BUILD-DICTIONARY` function, which takes no arguments and returns a hash table of all program
objects keyed by name.

The only thing left to do, is make use of the shader program to do your rendering. This is done by
issuing calls to the various `UNIFORM-*` functions within the body of the `WITH-PROGRAM` macro:

```lisp
(with-program :example-program
  (uniform-mat4 :mvp *matrix*))
```

Here, we specify that we want to use `:example-program` during rendering, modifying a single 4x4
matrix uniform value. Here `*matrix*` refers to an imaginary matrix that you should have created for
the object you wish to render. There are quite a few `UNIFORM-*` functions, and the full list can be
viewed in the [package's exported symbols](src/package.lisp). Note that each uniform
function takes the name of a uniform variable as a keyword symbol, followed by the value to modify
it with.

## License

Copyright © 2018 Michael Fiano <michael.fiano@gmail.com>.

Licensed under the MIT License.

A copy of the license is available [here](LICENSE).
