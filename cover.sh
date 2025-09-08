#!/usr/bin/env bash
find . -name '*.coverage' | xargs rm -f
dune runtest --instrument-with bisect_ppx --force
