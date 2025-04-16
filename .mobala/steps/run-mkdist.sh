#!/usr/bin/env bash

set -euo pipefail

function run-mkdist() {
  src="$(realpath "${MKDIST_SOURCE}")"
  tgt="$(realpath "${MKDIST_TARGET}")"
  
  distbin="$tgt/dist-bin"
  distzip="$tgt/dist-zip"
  
  mkdir -p $distbin
  mkdir -p $distzip
  
  pushd .
  
  cd $src
  
  for d in baboon-*
  do
    if [[ -f ./$d/baboon ]]; then
      cp ./${d}/baboon ${distbin}/${d}
      zip -r9 -j ${distzip}/${d}.zip ${distbin}/${d}
    elif [[ -f ./$d/baboon.exe ]]; then
      cp ./${d}/baboon.exe ${distbin}/${d}.exe
      zip -r9 -j ${distzip}/${d}.zip ${distbin}/${d}.exe
    else
      false
    fi
  done
  
  popd   
}
