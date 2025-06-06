#!/usr/bin/env python3

import pandas as pd
import re

df = pd.read_csv("m64467e_230327_154324_frequency_extracted_sequences_filtered.csv")

# to extract the gene
def extract_gene(sequence):
    pattern = r"ACCACTGACGAGCAGATTTC(.*?)GTATTGCGACGAATTGCCAC"
    match = re.search(pattern, sequence)
    if match:
        return match.group(1)
    else:
        return None

df['gene'] = df['read'].apply(extract_gene)
new_df = df[['gene', 'frequency', 'extracted_sequence']]
new_df.to_csv('gene_frequency_extracted.csv', index=False)

