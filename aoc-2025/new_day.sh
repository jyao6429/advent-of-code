#!/bin/bash

day=$1
touch "resources/day${day}.txt"
touch "resources/day${day}_example.txt"
cp "lib/dayN.ml" "lib/day${day}.ml"
sed -i "s/dayN/day${day}/" "lib/day${day}.ml"
