#!/usr/bin/env zsh

HERE="${0:A:h}"

set -eu

prefix="$HERE/default64.sml"

if [ ! -f "$prefix" ]; then
  echo "Error: $prefix does not exist" >&2
  exit 1
fi

rm -rf "$HERE/programs64"
mkdir -p "$HERE/programs64"
cp -r "$HERE/programs/DATA" "$HERE/programs64/DATA"

find "$HERE/programs" -maxdepth 1 -type f -name '*.sml' ! -name "profileit.sml" -print0 |
while IFS= read -r -d '' file; do
  base=$(basename "$file")
  cat "$prefix" "$file" > "$HERE/programs64/$base"
  echo "end (* default64 *)" >> "$HERE/programs64/$base"
  echo "Updated: $file"
done
cp -r "$HERE/programs/profileit.sml" "$HERE/programs64/"
