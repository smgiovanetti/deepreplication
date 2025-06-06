#!/usr/bin/env python3

import pandas as pd

df = pd.read_csv('classified_barcodes_without_gene.csv')

# sum 'frequency' for matching 'barcode', 'classification_sequence', 'classification', and 'barcode_length'
consolidated_df = df.groupby(['barcode', 'classification_sequence', 'classification', 'barcode_length'])['frequency'].apply(list).reset_index()
consolidated_df['sum_of_frequencies'] = consolidated_df['frequency'].apply(lambda x: sum(x))
output_file = 'grouped_output.csv'
consolidated_df.to_csv(output_file, index=False)
