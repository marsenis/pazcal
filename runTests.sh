#!/bin/bash

for i in examples/*.pzc
do
   echo "----> " $i
   ./pazcal $i >/dev/null
done
