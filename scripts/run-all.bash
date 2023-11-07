#!/bin/bash

for day in ./bin/*; do
  if [ "${day##*.}" = "ml" ]; then
    echo "----------- $day -----------"
    name="${day##*/}"
    name="${name%.*}"
    dune exec "$name"
    echo -e "\n"
  fi
done