import csv
import sys

def txt_to_csv(txt_file, csv_file):
    with open(txt_file, 'r') as infile, open(csv_file, 'w', newline='') as outfile:
        writer = csv.writer(outfile)
        for line in infile:
            # Split by tab (or modify this line if your txt file uses another delimiter)
            row = line.strip().split('\t')
            writer.writerow(row)

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python script.py <input_txt_file> <output_csv_file>")
        sys.exit(1)

    txt_file = sys.argv[1]  # Input text file from argument
    csv_file = sys.argv[2]  # Output CSV file from argument

    txt_to_csv(txt_file, csv_file)
    print(f'Conversion from {txt_file} to {csv_file} completed.')
