import sys
import pandas as pd
import pickle

query_inpath = sys.argv[1] + '_protein_change.csv'
Query_DF = pd.read_csv(query_inpath)
# Query_DF = pd.read_csv('protein_change_samita.csv')
Query_DF = Query_DF[~(Query_DF['Query'].isna())]
Query_DF
Query_list = Query_DF['Query'].tolist()
import pandas as pd
import pickle
# Function to load the query_dict from file
def load_query_dict(file_path):
    with open(file_path, 'rb') as f:
        query_dict = pickle.load(f)
    return query_dict
# Individual processing functions (already provided)


def Clinvar_process_queries(queries, query_dict):
    if isinstance(queries, str):
        result = query_dict.get(queries, {'ClinicalSignificance': 'Not found', 'ReviewStatus': 'Not found'})
        return pd.DataFrame({'Query': [queries], 'ClinicalSignificance': [result['ClinicalSignificance']], 'ReviewStatus': [result['ReviewStatus']]})
    elif isinstance(queries, list):
        results = []
        for query in queries:
            result = query_dict.get(query, {'ClinicalSignificance': 'Not found', 'ReviewStatus': 'Not found'})
            results.append({'Query': query, 'ClinicalSignificance': result['ClinicalSignificance'], 'ReviewStatus': result['ReviewStatus']})
        return pd.DataFrame(results)
    else:
        raise ValueError("Input must be a string or a list of strings")


def CKG_process_queries(queries, query_dict):
    if isinstance(queries, str):
        result = query_dict.get(queries, {'Drug': 'Not found', 'evidence': 'Not found', 'response': 'Not found'})
        return pd.DataFrame({'Query': [queries], 'Drug': [result['Drug']], 'Evidence': [result['evidence']], 'Response': [result['response']]})
    elif isinstance(queries, list):
        results = []
        for query in queries:
            result = query_dict.get(query, {'Drug': 'Not found', 'evidence': 'Not found', 'response': 'Not found'})
            results.append({'Query': query, 'Drug': result['Drug'], 'Evidence': result['evidence'], 'Response': result['response']})
        return pd.DataFrame(results)
    else:
        raise ValueError("Input must be a string or a list of strings")
def Oncokb_process_queries(queries, oncokb_csv_path):
    """
    Processes queries using data from an OncoKB CSV file.
    Parameters:
    - queries (str or list): Query string or list of query strings.
    - oncokb_csv_path (str): Path to the OncoKB CSV file containing the query data.
    Returns:
    - pd.DataFrame: DataFrame containing the results for all queries.
    """
    # Load the OncoKB CSV file into a DataFrame
    oncokb_df = pd.read_csv(oncokb_csv_path)
    # Filter the DataFrame based on the queries
    if isinstance(queries, str):
        result = oncokb_df[oncokb_df['Query'] == queries]
        if result.empty:
            # Return a DataFrame with "Not found" values if the query is not present
            return pd.DataFrame({'Query': [queries], 'Drug': ['Not found'], 'evidence': ['Not found'], 'response': ['Not found'], 'disease': ['Not found'], 'FDA_levels': ['Not found'], 'therapeutic_level': ['Not found'], 'prognostic_level': ['Not found'], 'diagnostic_level': ['Not found'] })
        return result
    elif isinstance(queries, list):
        results = []
        for query in queries:
            result = oncokb_df[oncokb_df['Query'] == query]
            if result.empty:
                # Append "Not found" values if the query is not present
                results.append({'Query': query, 'Drug': 'Not found', 'evidence': 'Not found', 'response': 'Not found', 'disease': 'Not found', 'FDA_levels': 'Not found', 'Therapeutic_level': 'Not found',  'Prognostic_level': 'Not found', 'Diagnostic_level': 'Not found'})
            else:
                results.append(result.to_dict('records')[0])
        return pd.DataFrame(results)
    else:
        raise ValueError("Input must be a string or a list of strings")


def Cosmic_process_queries_from_pickle(pickle_path, query_list):
    """
    Processes queries using a dictionary loaded from a pickle file.
    Parameters:
    - pickle_path (str): Path to the pickle file containing the query dictionary.
    - query_list (list): List of queries to be processed.
    Returns:
    - pd.DataFrame: DataFrame containing the results for all queries.
    """
    # Load the pickle file
    query_dict = pd.read_pickle(pickle_path)
    # Initialize an empty list to store the individual DataFrames
    dfs = []
    # Iterate over the list of queries
    for query in query_list:
        result = query_dict.get(query)
        if result is not None:
            result['Query'] = query
        else:
            # Create a dictionary with "Not found" values for missing queries
            result = {'Query': query, 'Its_Nucleotide_Query': 'Not found', 'LEGACY_MUTATION_ID': 'Not found', 'CLINVAR_CLNSIG': 'Not found', 'CLINVAR_TRAIT': 'Not found', 'Mutation_CDS': 'Not found'}
        # Convert the dictionary to a DataFrame and append to the list
        df = pd.DataFrame([result])
        dfs.append(df)
    # Concatenate all the individual DataFrames into a single DataFrame
    final_df = pd.concat(dfs, ignore_index=True)
    return final_df
# def process_all_queries(query_list, clinvar_dict, ckg_dict, oncokb_csv_path, cosmic_pickle_path, output_file):
#     # Process each query set
#     clinvar_results = Clinvar_process_queries(query_list, clinvar_dict)
#     ckg_results = CKG_process_queries(query_list, ckg_dict)
#     oncokb_results = Oncokb_process_queries(query_list, oncokb_csv_path)
#     cosmic_results = Cosmic_process_queries_from_pickle(cosmic_pickle_path, query_list)
#     # Merge results on the 'Query' column
#     combined_results = pd.merge(clinvar_results, ckg_results, on='Query', how='outer')
#     combined_results = pd.merge(combined_results, oncokb_results, on='Query', how='outer')
#     combined_results = pd.merge(combined_results, cosmic_results, on='Query', how='outer') 
#     print('Processing Done')
    # Merge Drug, Response, and Disease into treatment columns
    # combined_results['treatment'] = combined_results.apply(lambda row: 
    #     "" if (row['Drug_x'] == "Not found" and row['Response'] == "Not found" and 
    #             row['disease_x'] == "Not found" and row['Drug_y'] == "Not found" and 
    #             row['response_y'] == "Not found" and row['disease_y'] == "Not found" and 
    #             row['Drug'] == "Not found" and row['response_y'] == "Not found") else
    #     f"{row['Drug_x']}: {row['Response']}\n" +
    #     f"{row['disease_x']}: {row['Drug_y']}: {row['response_x']}\n" +
    #     f"{row['disease_y']}: {row['Drug']}: {row['response_y']}", axis=1)
    # combined_results['treatment'] = combined_results.apply(lambda row: 
    #     f"{row['Drug_x']}: {row['Response']}\n" +
    #     f"{row['disease_x']}: {row['Drug_y']}: {row['response_x']}\n" +
    #     f"{row['disease_y']}: {row['Drug']}: {row['response_y']}", axis=1)
    # combined_results.drop(columns=['Drug_x','Evidence','Response', 'Drug_y','evidence_x','response_x','disease_x', 'Drug' ,'evidence_y','response_y','disease_y'], inplace=True) 
    # Save to CSV
    # combined_results.to_csv(output_file, index=False)
    # print('Results saved in a CSV:', output_file)
import numpy as np
def process_all_queries(query_list, clinvar_dict, ckg_dict, oncokb_csv_path, cosmic_pickle_path, output_file):
    # Process each query set
    clinvar_results = Clinvar_process_queries(query_list, clinvar_dict)
    ckg_results = CKG_process_queries(query_list, ckg_dict)
    oncokb_results = Oncokb_process_queries(query_list, oncokb_csv_path)
    cosmic_results = Cosmic_process_queries_from_pickle(cosmic_pickle_path, query_list)
    # Merge results on the 'Query' column
    combined_results = pd.merge(clinvar_results, ckg_results, on='Query', how='outer')
    combined_results = pd.merge(combined_results, oncokb_results, on='Query', how='outer')
    combined_results = pd.merge(combined_results, cosmic_results, on='Query', how='outer') 
    # combined_results.replace(['Not found', 'Not provided', np.nan], np.nan, inplace=True)
    print('Processing Done')
    # Merge Drug, Response, and Disease into treatment columns
    combined_results['treatment'] = combined_results.apply(lambda row: 
        "" if (row['Drug_x'] == "Not found" and row['Evidence'] == "Not found" and 
                row['Response'] == "Not found" and row['Drug_y'] == "Not found" and 
                row['evidence'] == "Not found" and row['response'] == "Not found" and 
                row['disease'] == "Not found" 
                # and row['response_y'] == "Not found"
                ) 
                else
                f"{row['Drug_x']}: {row['Evidence']}: {row['Response']}\n" +
        # f"{row['Drug_x']}: {row['Response']}\n" +
        f"{row['Drug_y']}: {row['evidence']}: {row['response']}: {row['disease']}", axis=1)
    # combined_results['treatment'] = combined_results.apply(lambda row: 
    # "" if (row['Drug_x'] in ["Not found", "nan"] and 
    #         row['Response'] in ["Not found", "nan"] and 
    #         row['disease_x'] in ["Not found", "nan"] and 
    #         row['Drug_y'] in ["Not found", "nan"] and 
    #         row['response_y'] in ["Not found", "nan"] and 
    #         row['disease_y'] in ["Not found", "nan"] and 
    #         row['Drug'] in ["Not found", "nan"] and 
    #         row['response'] in ["Not found", "nan"]) else
    # f"{row['Drug_x']}: {row['Response']}\n" +
    # f"{row['disease_x']}: {row['Drug_y']}: {row['response_x']}\n" +
    # f"{row['disease_y']}: {row['Drug']}: {row['response_y']}", axis=1)
    # combined_results['treatment'] = combined_results.apply(lambda row: 
    # "" if (row['Drug_x'] in ["Not found", None] or pd.isna(row['Drug_x']) and 
    #        row['Response'] in ["Not found", None] or pd.isna(row['Response']) and 
    #        row['disease_x'] in ["Not found", None] or pd.isna(row['disease_x']) and 
    #        row['Drug_y'] in ["Not found", None] or pd.isna(row['Drug_y']) and 
    #        row['response_x'] in ["Not found", None] or pd.isna(row['response_x']) and 
    #        row['disease_y'] in ["Not found", None] or pd.isna(row['disease_y']) and 
    #        row['Drug'] in ["Not found", None] or pd.isna(row['Drug']) and 
    #        row['response_y'] in ["Not found", None] or pd.isna(row['response_y'])) else
    # f"{row['Drug_x']}: {row['Response']}\n" +
    # f"{row['disease_x']}: {row['Drug_y']
    # Merge Drug, Response, and Disease into treatment columns
    # combined_results['treatment'] = combined_results.apply(lambda row: 
    #     f"{row['Drug_x']}: {row['Response']}\n" +
    #     f"{row['disease_x']}: {row['Drug_y']}: {row['response_x']}\n" +
    #     f"{row['disease_y']}: {row['Drug']}: {row['response_y']}", axis=1)
    # combined_results['treatment_x'] = combined_results[['Drug_x', 'Response']].apply(lambda row: f"{row['Drug_x']}: {row['Response']}", axis=1)
    # combined_results['treatment_y'] = combined_results[['Drug_y', 'response_x', 'disease_x']].apply(lambda row: f"{row['disease_x']}: {row['Drug_y']}: {row['response_x']}", axis=1)
    # combined_results['treatment_z'] = combined_results[['Drug', 'response_y', 'disease_y']].apply(lambda row: f"{row['disease_y']}: {row['Drug']}: {row['response_y']}", axis=1)
    # Drop the 'evidence' columns and original 'Drug', 'Response', and 'Disease' columns
    combined_results.drop(columns=['Drug_x','Evidence','Response', 'Drug_y','evidence','response','disease'], inplace=True) 
    # Save to CSV
    combined_results.to_csv(output_file, index=False)
    print('Results saved in a CSV:', output_file)


file_path_clinvar = 'KG-files/ClinVAr_query_dict.pkl'
file_path_ckg = 'KG-files/CKG_gene_query_dict_dedup.pkl'
oncokb_csv_path = 'KG-files/onkokb_final.csv'
file_path_cosmic = 'KG-files/COSMIC_query_dict_1.pkl'
# Load dictionaries
clinvar_dict = load_query_dict(file_path_clinvar)
ckg_dict = pd.read_pickle(file_path_ckg)
# oncokb_dict = pd.read_pickle(file_path_oncokb)

# Read queries from input
query_inpath = sys.argv[1] + '_protein_change.csv'
Query_DF = pd.read_csv(query_inpath)
Query_DF = Query_DF[~(Query_DF['Query'].isna())]
Query_list = Query_DF['Query'].tolist()
process_all_queries(Query_list, clinvar_dict, ckg_dict, oncokb_csv_path, file_path_cosmic, sys.argv[1] + '_kg_results.csv')
