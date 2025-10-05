import csv
import argparse

# Set up argument parsing
parser = argparse.ArgumentParser(description='Convert .cns file to .csv format.')
parser.add_argument('input_file', type=str, help='Path to the input .cns file')
parser.add_argument('output_file', type=str, help='Path to the output .csv file')

# Parse the arguments
args = parser.parse_args()

# Open the .cns file and read the data
with open(args.input_file, 'r') as infile:
    lines = infile.readlines()

# Split the lines into headers and data
header = lines[0].strip().split('\t')  # First line as header
data = [line.strip().split('\t') for line in lines[1:]]  # Remaining lines as data

# Write the data to a CSV file
with open(args.output_file, 'w', newline='') as csvfile:
    writer = csv.writer(csvfile)
    
    # Write header
    writer.writerow(header)
    
    # Write data rows
    writer.writerows(data)

print(f"Data has been saved to {args.output_file}")
