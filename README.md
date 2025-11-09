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

## freetype

the road to `freetype2` is long and full of sorrow.
if you are fearless enough, then do the following:

1. Clone [`storable-offset`](https://github.com/BurningWitness/storable-offset.git), the repository for a very small dependency for `freetype2`.

1. In `storable-offset.cabal`, change `build-depends: base >=4.10 && <4.17` to `build-depends: base >=4.10`.

1. Clone [`freetype2`](https://github.com/dagit/freetype2.git), the repository for the Haskell bindings to FreeType. We need a local installation, for we shall alter its source code.

1. In `/freetype2`, make a file named `cabal.project` and write to it the following:
    ```hs
        packages: .
                  /path/to/storable-offset
    ```
    This tells the compiler to build the project with the local installation of `storable-offset`.

1. In `freetype2.cabal`, change all four instances of `base >= 4.10 && < *.**` to `base >= 4.10`.


1. In `src/gzip/ftzconf.h`, make the following change ([source](https://github.com/solvespace/solvespace/issues/1539#issuecomment-2694149011)):
    ```h
    - #if !defined(MACOS) && !defined(TARGET_OS_MAC)
    + #if !defined(__MACTYPES__)
    ```

1. In ``Format/BDF/Types.hsc`, make the following change:
    ```h
    - #ifndef aarch64_HOST_ARCH
      import           Data.Int
    - #endif
    ```
    This tells Haskell to import `Data.Int`, whatever the architecture of the machine.

1. In each of the following files:
    - `Support/Modules/Types.hsc`
    - `Support/Modules/Internal.hsc`
    - `Format/Font/Internal.hsc`
    - `Format/CID/Internal.hsc`
    - `Format/BDF/Internal.hsc`
    - `Error/Enumerations/Internal.hsc`
    - `Core/Base/Internal.hsc`

    Make the following change:
    ```h
    - #ifdef aarch64_HOST_ARCH
    - import           Data.Word
    - #else
      import           Data.Int
    - #endif
    ```
    This tells Haskell to import `Data.Int` rather than `Data.Word`, whatever the architecture of the machine.

1. In `/frogwork`, make a file named `cabal.project` and write to it the following:
    ```hs
    packages: .
              /path/to/freetype2
              /path/to/storable-offset
    ```
    This tells the compiler to build the project with the local installations of `freetype2` and `storable-offset`.

1. In `/frogwork`, run `cabal build` and pray to every god.

## profiling
```sh
cabal configure --enable-profiling
cabal run frogwork -- +RTS -pj -RTS
```
