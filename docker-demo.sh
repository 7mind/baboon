#!/usr/bin/env bash

set -xe

OUT=./baboon-docker.tar

nix build '.#baboon-container' --out-link "${OUT}"  

docker load --input "${OUT}"

IMAGEID=$(tar -xzOf "${OUT}" manifest.json | jq -r '.[].RepoTags[0]')

docker run -ti -v ./src/test/resources/baboon:/input/:z --rm "${IMAGEID}" --output /tmp/1 --model-dir /input   

rm "${OUT}"