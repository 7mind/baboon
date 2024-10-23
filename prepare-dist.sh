#!/usr/bin/env bash

set -x
set -e


src=$1

mkdir -p dist-bin
mkdir -p dist-zip

distbin="$(pwd)/dist-bin"
distzip="$(pwd)/dist-zip"

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
