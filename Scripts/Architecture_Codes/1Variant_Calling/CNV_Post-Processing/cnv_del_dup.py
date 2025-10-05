import pandas as pd
import argparse

# Set up argument parsing
parser = argparse.ArgumentParser(description='Classify CNV as DEL/DUP based on log2 values.')
parser.add_argument('input_file', type=str, help='Path to the input CSV file')
parser.add_argument('output_file', type=str, help='Path to the output CSV file')

# Parse the arguments
args = parser.parse_args()

# Read the input CSV file
df = pd.read_csv(args.input_file)

# Define a function to classify CNV based on log2 ratio
def classify_cnv(log2_value):
    if log2_value < 0:
        return 'DEL'
    elif log2_value > 0:
        return 'DUP'
    else:
        return 'Neutral'

# Apply the function to the 'log2' column to create the 'DUP/DEL' column
df['DUP/DEL'] = df['log2'].apply(classify_cnv)

# Save the updated dataframe to a new CSV file
df.to_csv(args.output_file, index=False)

print(f"Updated CSV file saved to {args.output_file}")
