#!/usr/bin/env bash

set -euo pipefail
if [[ "${DO_VERBOSE}" == 1 ]] ; then set -x ; fi

function run-fmt() {
if [[ "$DO_FMT" == 1  ]]; then
  cs launch scalafmt -- $@   
  git add . || true 
fi
}
