import pandas as pd
import argparse

# Set up argument parsing
parser = argparse.ArgumentParser(description='Convert CSV to BED format.')
parser.add_argument('input_file', type=str, help='Path to the input CSV file')
parser.add_argument('output_file', type=str, help='Path to the output BED file')

# Parse the arguments
args = parser.parse_args()

# Read the input CSV file
df = pd.read_csv(args.input_file)

# Select the columns for the BED file format: chromosome, start, end, and 'DUP/DEL'
# Make sure these column names exist in your input CSV file
bed_df = df[['chromosome', 'start', 'end', 'DUP/DEL']]

# Save the selected columns as a BED file (tab-separated without headers and index)
bed_df.to_csv(args.output_file, sep='\t', header=False, index=False)

print(f"BED file saved to {args.output_file}")
