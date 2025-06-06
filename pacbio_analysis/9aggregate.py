#!/usr/bin/env python3

import pandas as pd

df = pd.read_csv('rad14negshort_incorrectseq.csv')
consolidated_df = df.groupby(['barcode', 'classification', 'barcode_length', 'gene'])['frequency'].apply(list).reset_index()
consolidated_df['sum_of_frequencies'] = consolidated_df['frequency'].apply(lambda x: sum(x))
output_file = 'rad14negshort_incorrectseq_grouped.csv'
consolidated_df.to_csv(output_file, index=False)
