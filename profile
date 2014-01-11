#!/usr/bin/env zsh

./.hsenv/bin/cabal configure -fprofile --enable-executable-profiling &&
./.hsenv/bin/cabal build &&
dist/build/charles1/charles1
