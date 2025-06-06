#!/usr/bin/env python3
#used to subset 'grouped_output.csv' and 'classified_barcodes_xx.csv'
import pandas as pd

df = pd.read_csv('classified_barcodes_with_gene.csv')

unique_classifications = df['classification'].unique()

for classification in unique_classifications:
    subset_df = df[df['classification'] == classification].copy()
    file_name = f"{classification}_gene.csv"
    subset_df.to_csv(file_name, index=False)
