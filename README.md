# unlambda
![general version](http://img.shields.io/badge/version-0.1.1-green.svg)
![GPL Licensed](http://img.shields.io/badge/license-GPLv2-blue.svg)
[![Build Status](https://travis-ci.org/hellerve/unlambda.png?branch=master)](https://travis-ci.org/hellerve/unlambda)

An Unlambda Interpreter written in Haskell. It's basically a cleaned-up version
of [this](https://github.com/bwo/unlambda/blob/master/unlambda.hs), where I
added the REPL and the cabal files.

## Building & Installing

Unlambda has a cabal toolchain, which means you can install it by issuing `cabal install`
if you have the tool installed.

## Running

You can run files like with any regular interpreter by using the command 
`unlambda yourfile` or go into interactive mode by running unlambda without
any other arguments. You will be greeted by your run-of-the-mill shell.

Have fun!
