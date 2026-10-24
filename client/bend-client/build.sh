#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
bend_bin=${BEND:-bend}
if [[ $("$bend_bin" version) != 'bend 2.0.39' ]]; then
  echo 'bend-client requires Bend 2.0.39 (see README.md).' >&2
  exit 1
fi
mkdir -p build
"$bend_bin" PROOF.bend --check-only
"$bend_bin" src/main.bend --check-only
"$bend_bin" src/main.bend -o build/bend-client
