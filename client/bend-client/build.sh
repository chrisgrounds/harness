#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
bend_bin=${BEND:-bend}
if [[ $("$bend_bin" version) != 'bend 2.0.39' ]]; then
  echo 'bend-client requires Bend 2.0.39 (see README.md).' >&2
  exit 1
fi
if ! pkg-config --atleast-version=0.19 json-c; then
  echo 'bend-client requires json-c 0.19 or newer for strict UTF-8 JSON parsing.' >&2
  exit 1
fi
mkdir -p build
"$bend_bin" src/main.bend -o build/client.c
# Bend emits its runtime and foreign effects as one translation unit.
read -r -a json_flags <<< "$(pkg-config --cflags --libs json-c)"
"${CC:-clang}" -std=c11 -O2 build/client.c -pthread -lm "${json_flags[@]}" -o build/bend-client
