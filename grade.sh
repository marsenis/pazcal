for f in chara/*.pzc; do
   echo "$f"
   ./pazcal "$f"
   BASE="${f%.*}"
   INPUT="${BASE}.in"
   OUTPUT="${BASE}.out"
   if [ -e "$INPUT" ]; then
      "$BASE" < "$INPUT" > tmp
   else
      "$BASE" > tmp
   fi
   echo "Our output: "
   cat tmp
   echo "Chara's output: "
   cat "$OUTPUT"
   diff -qZ tmp "$OUTPUT"
   echo "Next? "
   read -r line
done
