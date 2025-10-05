import sys

# Get input filename
fi = sys.argv[1] + "_bed_filtered.merged2"

# Define input/output files
input_vcf = fi + ".vcf"
output_csv = fi + ".AFadded.csv"

# Open output CSV
with open(output_csv, "w") as fout:
    fout.write("CHRM\tLOC\tRSID\tRef\tALt\tRefAF\tAltAF\tINFO\n")

    with open(input_vcf) as fin:
        for line in fin:
            if line.startswith("#"):
                continue  # Skip header lines

            # Split VCF columns
            columns = line.strip().split("\t")
            chrom, pos, rsid, ref, alt, _, _, info, fmt, sample_data = columns[:10]

            # Parse INFO field as a dictionary
            info_dict = {kv.split("=")[0]: kv.split("=")[1] for kv in info.split(";") if "=" in kv}

            # Extract DP (Total Read Depth) from INFO field
            dp = int(info_dict.get("DP", 0))  # Default to 0 if missing

            # Extract FORMAT fields (GT:AD:AF:DP:...)
            fmt_fields = fmt.split(":")
            sample_values = sample_data.split(":")
            fmt_dict = dict(zip(fmt_fields, sample_values))

            # Extract allele depth (AD) and allele frequency (AF)
            ad = fmt_dict.get("AD", "0,0").split(",")  # Reference and alternate depths
            af = fmt_dict.get("AF", "0")  # Already computed AF in VCF

            # Convert AD to integers
            ad = [int(allele) for allele in ad]

            # Calculate AF values if not provided in AF field
            if dp > 0:
                ref_af = ad[0] / dp  # Reference Allele Frequency
                alt_afs = [str(alt / dp) for alt in ad[1:]]  # List of Alt Allele Frequencies
                alt_af_str = ",".join(alt_afs) if alt_afs else "0"
            else:
                ref_af, alt_af_str = "NA", "NA"  # Handle zero depth case

            # Write to file
            fout.write(f"{chrom}\t{pos}\t{rsid}\t{ref}\t{alt}\t{ref_af}\t{alt_af_str}\t{info}\n")