<div align="center">
    <h3><em>frogwork</em></h3>
    current goal: survival horror platformer with cute 3d frogs
</div>

---

languages: <br>
✦ haskell<br>
✦ glsl<br>
libraries:<br>
✦ sdl2<br>
✦ opengl<br>

# setup

[ghcup](https://www.haskell.org/ghcup/) + [cabal](https://www.haskell.org/cabal/)
```sh
cabal run
```

# fand
```sh
cabal configure --enable-profiling
cabal run frogwork -- +RTS -pj -RTS
```
