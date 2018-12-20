#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage 'html2png.sh <inputdir>'"
    exit 1
fi

DIR=$1

for f in $DIR/*.html; do
    bn=`basename "$f"`
    screenshot_file="$bn.png"
    google-chrome --headless --screenshot="$screenshot_file" --window-size=800,800 "$f"
    mv "$screenshot_file" "$DIR"
done
