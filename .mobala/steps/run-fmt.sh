#!/usr/bin/env bash

set -euo pipefail

function run-fmt() {
  cs launch scalafmt -- $@   
  git add . || true 
}
