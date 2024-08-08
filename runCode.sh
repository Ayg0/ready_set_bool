#!/bin/bash

# Check if a file name is provided
if [ $# -eq 0 ]; then
  echo "Usage: $0 <Haskell source file>"
  exit 1
fi

# Get the file name from the first argument
FILE=$1

# Compile the Haskell code
ghc -no-keep-hi-files -no-keep-o-files "$FILE"

# Check if the compilation was successful
if [ $? -eq 0 ]; then
  # Extract the base name without extension for the executable
  BASE_NAME=$(basename "$FILE" .hs)
  # Run the compiled program
  ./"$BASE_NAME"
else
  echo "Compilation failed."
fi
