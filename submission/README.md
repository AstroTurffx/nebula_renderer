### VISIT THE REPO PLEASE
The provided `Main.hs` is a compacted version where all of the code is put into one file and does not feature drawing the background, if u want the background feature or would like to see more renders (and the README is quite informative) please visit [the repo](https://github.com/AstroTurffx/nebula_renderer)

# How to run 
Install `gloss` and `pure-noise` with
```
cabal install gloss --lib
cabal install pure-noise --lib
```
Build with
```
ghc Main.hs -O2  -threaded -rtsopts -with-rtsopts=-N -package base -package bytestring -package time -package array -package gloss
```
and run the executable, `Main.exe` or `./Main`

# Nebula Raymarching
A multi-density volumetric CPU renderer which utilises raymarcing and multi-threading. Written in haskell and made for the INF1A FP Competition at the University of Edinburgh