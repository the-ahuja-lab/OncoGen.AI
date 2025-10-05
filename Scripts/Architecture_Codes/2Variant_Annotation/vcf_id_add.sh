#!/bin/bash

# Input VCF file
input_file=$1

# Output VCF file
output_file=$2

# Initialize variant counter
variant_number=1

# Check if the output file exists and remove it
if [[ -f "$output_file" ]]; then
  rm "$output_file"
  echo "Removed existing file: $output_file"
fi

# Process the input file
while read -r line; do
  # Check if the line is a header
  if [[ "$line" == "#"* ]]; then
    # Print the header lines as they are
    echo "$line" >> "$output_file"
  else
    # Split the line into columns using tab delimiter
    IFS=$'\t' read -r -a columns <<< "$line"

    # Assign a unique ID to the variant, overwriting the current ID
    columns[2]="var$variant_number"

    # Increment variant number
    ((variant_number++))

    # Join the columns back together and append to output file
    printf "%s\n" "${columns[*]}" | tr ' ' '\t' >> "$output_file"
  fi
done < "$input_file"

echo "Processing completed. Output written to $output_file"
