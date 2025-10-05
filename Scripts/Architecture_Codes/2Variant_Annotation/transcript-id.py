import pandas as pd
import argparse

# Function to parse arguments
def parse_args():
    parser = argparse.ArgumentParser(description="Filter oncogen data based on MANE_Select NM_ID, Ensembl IDs, and CCDS entries.")
    parser.add_argument('input_file', type=str, help="Path to the input CSV file.")
    parser.add_argument('apris_file', type=str, help="Path to the APRIS data file.")
    parser.add_argument('output_file', type=str, help="Path to save the output CSV file.")
    return parser.parse_args()

# Main function
def main(input_file, apris_file, output_file):
    # Load the data from the CSV files
    oncogen_df = pd.read_csv(input_file, low_memory=False)
    apris_df = pd.read_csv(apris_file)

    # Filter APRIS data to only keep MANE_Select entries
    apris_mane = apris_df[apris_df['MANE'] == 'MANE_Select']

    # Create dictionaries for faster lookup
    ensembl_to_mane = dict(zip(apris_mane['Transcript ID'].str.split('.').str[0], apris_mane['refseq_mrna'].str.split('.').str[0]))
    ccds_to_mane = dict(zip(apris_mane['CCDS ID'], apris_mane['refseq_mrna'].str.split('.').str[0]))

    # Function to keep only the MANE_Select Ensembl ID in the 'Feature' column
    def filter_feature_ensembl(feature, ccds):
        ensembl_ids = feature.split(',')
        matching_ids = [eid for eid in ensembl_ids if eid.split('_')[0] in ensembl_to_mane]
        if not matching_ids and ccds != '-':
            if ccds in ccds_to_mane:
                return feature
        return ','.join(matching_ids) if matching_ids else None

    # Apply filtering based on 'Feature' column and check for CCDS match
    oncogen_df['Feature'] = oncogen_df.apply(lambda row: filter_feature_ensembl(row['Feature'], row['CCDS']), axis=1)

    # Drop rows where no Ensembl ID matched in 'Feature'
    oncogen_df = oncogen_df.dropna(subset=['Feature'])

    # Function to filter and keep only matching Ensembl Transcript IDs in the 'AAChange.ensGene.x' column
    #def filter_ensembl_ids(ensembl_changes):
    #    ensembl_ids = ensembl_changes.split(',')
    #    matching_ids = []
    #    for eid in ensembl_ids:
    #        parts = eid.split(':')
    #        if len(parts) > 1:
    #            ensembl_id = parts[1].split('_')[0].split('.')[0]
    #            if ensembl_id in ensembl_to_mane:
    #                matching_ids.append(eid)
    #    return ','.join(matching_ids) if matching_ids else None

    # Apply filtering based on 'AAChange.ensGene.x'
    #oncogen_df['AAChange.ensGene.x'] = oncogen_df['AAChange.ensGene.x'].apply(filter_ensembl_ids)

    # Drop rows where no Ensembl ID matched in 'AAChange.ensGene.x'
    #oncogen_df = oncogen_df.dropna(subset=['AAChange.ensGene.x'])

    # Function to filter and keep only matching NM_ID entries in the 'AAChange.refGene.x' column
    def filter_refgene_ids(refgene_changes, index):
        print(f"Processing variant number: {index}")  # Print the variant number (index)
        refgene_ids = refgene_changes.split(',')
        matching_ids = []
        for rid in refgene_ids:
            parts = rid.split(':')
            if len(parts) > 1:
                nm_id = parts[1].split('.')[0]
                if nm_id in apris_mane['refseq_mrna'].str.split('.').str[0].values:
                    matching_ids.append(rid)
        return ','.join(matching_ids) if matching_ids else None
    
    oncogen_df['AAChange.refGene.x'] = oncogen_df['AAChange.refGene.x'].fillna('.')
    oncogen_df['AAChange.refGene.x'] = oncogen_df['AAChange.refGene.x'].fillna('.')


    # Apply filtering based on 'AAChange.refGene.x' and pass the index to the function
    oncogen_df['AAChange.refGene.x'] = oncogen_df.apply(lambda row: filter_refgene_ids(row['AAChange.refGene.x'], row.name), axis=1)

    # Drop rows where no NM_ID matched in 'AAChange.refGene.x'
    # oncogen_df = oncogen_df.dropna(subset=['AAChange.refGene.x'])

    # Remove the 'Feature' column
    # oncogen_df = oncogen_df.drop(columns=['Feature'])

    # Define the desired column order
    desired_order = [
        'Location', 'Ref', 'ALt', 'RefAF', 'AltAF', 'Ref.Gene', 'Amino_acids', 'Protein_position',
        'cDNA_position', 'Codons', 'EXON', 'Func.refGene.x', 'INTRON', 'AAChange.refGene.x',
        'VARIANT_CLASS', 'Upload_variation', 'InterVar..InterVar.and.Evidence', 'CLIN_SIG',
        'Feature_type', 'Consequence', 'BIOTYPE', 'SIFT', 'SIFT_score.x', 'SIFT4G_score', 'PolyPhen',
        'Polyphen2_HVAR_score', 'Polyphen2_HDIV_score', 'DOMAINS', 'AF', 'AFR_AF', 'AMR_AF', 'EAS_AF',
        'EUR_AF', 'SAS_AF', 'gnomADe_AF', 'gnomADe_AFR_AF', 'gnomADe_AMR_AF', 'gnomADe_ASJ_AF',
        'gnomADe_EAS_AF', 'gnomADe_FIN_AF', 'gnomADe_NFE_AF', 'gnomADe_OTH_AF', 'gnomADe_SAS_AF',
        'MAX_AF', 'MAX_AF_POPS', 'SOMATIC', 'PUBMED'
    ]

    # Rearrange columns according to the desired order
    remaining_columns = [col for col in oncogen_df.columns if col not in desired_order]
    final_order = desired_order + remaining_columns

    # Reorder the dataframe
    oncogen_df = oncogen_df[final_order]

    # Drop additional specified columns
    columns_to_drop = [
        'MANE_SELECT', 'MANE_PLUS_CLINICAL', 'TSL', 'APPRIS', 'SWISSPROT', 'UNIPROT_ISOFORM', 'miRNA', 
        'HGVSc', 'HGVSp', 'HGVS_OFFSET', 'MOTIF_NAME', 'MOTIF_POS', 'HIGH_INF_POS', 'MOTIF_SCORE_CHANGE', 
        'TRANSCRIPTION_FACTORS', 'Upload_variation2', 'CHRM', 'LOC', 'VCF_ID2', 'VCF_ID3', 'formatted_ID', 'AAChange.ensGene.x', 
        'ID', 'ID.1', 'Chr.x', 'Start.x', 'End.x', 'Ref.x', 'Alt.x', 'Gene.ensGene.x', 'avsnp147.x', 
        'clinvar..Clinvar', 'AAChange.knownGene.x', 'Chr.y', 'Start.y', 'End.y', 'Ref.y', 'Alt.y', 
        'Func.refGene.y', 'Gene.refGene', 'GeneDetail.refGene', 'ExonicFunc.refGene.y', 'AAChange.refGene.y', 
        'CLNSIG', 'Func.ensGene', 'Gene.ensGene.y', 'GeneDetail.ensGene', 'ExonicFunc.ensGene', 
        'AAChange.ensGene.y', 'Func.knownGene', 'Gene.knownGene', 'GeneDetail.knownGene', 
        'ExonicFunc.knownGene', 'AAChange.knownGene.y', 'SYMBOL', 'SYMBOL_SOURCE', 'HGNC_ID', 'Otherinfo1', 'Otherinfo2', 'Otherinfo3','location'
    ]
    
    oncogen_df = oncogen_df.drop(columns=[col for col in columns_to_drop if col in oncogen_df.columns])

    # Save the filtered dataframe to a new CSV file
    oncogen_df.to_csv(output_file, index=False)

    print("Filtering, rearranging, and dropping columns complete")

# Run the main function with parsed arguments
if __name__ == "__main__":
    args = parse_args()
    main(args.input_file, args.apris_file, args.output_file)
