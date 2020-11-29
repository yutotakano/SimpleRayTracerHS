# SimpleRayTracerHS

This is RayTracer built in Haskell, for the Functional Programming competition at University of Edinburgh. 

Details are a secret until the competition is over.

## Running

Compile using GHC:
```
ghc -O2 -fexcess-precision -optc-ffast-math -optc-O3 -rtsopts Main.hs
```
The options enable faster floating point math, more optimization, and a profiler (which you can show by adding the `+RTS -s` flag to the executable).

Either call the generated executable directly with the arguments below, or run on DICE using `nice`.

### Command-line arguments:

```
--shadow        Turns on the shadow if specified.
--noshadow-zoom If specified uses the no-shadow texture on Zoom. 
--frames        If specified only renderes those frames. Separated by comma.
--res 192:108   Specifies the output resolution.
```

## Libraries

- JuicyPixels

## Some measurements:

- Time it takes to render 1 frame 1920x1080, no shadows: ~3 minutes
- Time it takes to render 1 frame 1920x1080, with shadows: ~180 minutes