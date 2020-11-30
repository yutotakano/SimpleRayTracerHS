# SimpleRayTracerHS

This is a ray tracer built in Haskell, for the Functional Programming competition at University of Edinburgh. It generates a sequence of PNG images (to be combined for GIF) of a ray-trached world. It supports shadows, although it is computationally expensive at an exponential cost. The script can utilize all the CPUs available through concurrent threading.

`RayTracer.hs` contains the core RayTracing functionality, `Vector.hs` contains the vector module, and `Main.hs` contains the world configuration, as well as the concurrency configuration.

## Running

Compile using GHC:
```
ghc -O2 -fexcess-precision -optc-ffast-math -optc-O3 -threaded -rtsopts Main.hs
```
The options enable faster floating point math, more optimization, multi-thread compatibility, and a profiler (which you can show by adding the `+RTS -s` flag to the executable).

Due to the overhead of generating concurrent threads (I think), the script runs into an Internal Error (out of RAM) on most consumer-level hardware. It works on DICE (UoE computing servers), so I recommend anybody trying out to use that too.

Make sure to utilize available cores by using `+RTS -N<number>`, such as `+RTS -N64`.

### Command-line arguments:

```
--shadow        Turns on the shadow if specified.
--noshadow-zoom If specified uses the no-shadow texture on Zoom. 
                Useful for the generating the first render to put into the texture.
--frames        If specified only renderes those frames. Separated by comma.
                Ex: --frames 0,2,5
--res 192:108   Specifies the output resolution. If unspecified uses 960:540.
```

## Libraries

- JuicyPixels (for generating images)
- Async (for concurrency)

## Generated Images:

- 1920x1080, 1 frame, no shadows, laptop, multi-threaded, 5 minutes
![Image](https://i.imgur.com/P71RrG3.png)

- 960x540, 1 frame, shadows, DICE, multi-threaded, 173 minutes (single-threaded: 470 minutes)

![Image](https://i.imgur.com/vNUisyL.png)

- 960x540, GIF, no shadows, laptop, single-threaded, 110 minutes

File too large to display here. 