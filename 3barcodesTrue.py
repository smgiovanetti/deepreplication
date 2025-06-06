#!/usr/bin/env python3

import pandas as pd

# csv
file_path = 'm64467e_230327_154324_frequency_extracted_sequences.csv'
df = pd.read_csv(file_path)

# keep rows with anchors (column named 'contains_anchors')
filtered_df = df[df['contains_anchors'] == True]

filtered_file_path = 'm64467e_230327_154324_frequency_extracted_sequences_filtered.csv'
filtered_df.to_csv(filtered_file_path, index=False)

