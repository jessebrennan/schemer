# schemer

## overview

Schemer is (for lack of a better name) my implimentation of
[_Write Yourself a Scheme in 48 Hours_](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours)
(WYS48H) using [Stack](https://docs.haskellstack.org/en/stable/README/).
WYS48H is a tutorial in Haskell that teaches you to build an
interpreter for Scheme.

Only some of the exercises are completed.

## building

First clone the repo. Then, like any Stack project all you have to do
to build is run

```bash
stack build
```

## running

_WARNING:_ This project not complete nor fully functional. Running it
may not have to result you expect.

I told stack to call the executable `schemer` so to run it just type

```bash
stack exec schemer
```

## code

Most of the code is in [`Lib.hs`](src/Lib.hs) but there is a little bit
of relevant stuff to be found in [`Main.hs`](app/Main.hs).
