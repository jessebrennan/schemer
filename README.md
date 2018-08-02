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

_WARNING:_ This project is only mostly complete. Running it
may not have to result you expect.

I told stack to call the executable `schemer` so to run it just type

```bash
stack exec schemer
```

## code

~~Most of the code is in [`Lib.hs`](src/Lib.hs) but there is a little bit
of relevant stuff to be found in [`Main.hs`](app/Main.hs).~~

I tried to modularize a lot of the code with some success. All of the
error handling stuff is in [`src/Error.hs`](src/Error.hs). Parsing stuff
resides in [`src/Parse.hs`](src/Parse.hs). The basic data type for the
abstract syntax tree is in [`src/AST.hs`](src/AST.hs) along with a helper
function for which I was to lazy to make a `Util.hs` module. Finally
[`src/Eval.hs`](src/Eval.hs) contains all of the nitty gritty details for
evaluating the AST and executing the Scheme.

Also [`app/Main.hs`](app/Main.hs) has all of the IO related code that
runs the actual interpreter.
