#!/usr/bin/env python3

import pandas as pd

df = pd.read_csv('4_frequency_with_barcodes.csv')
collapsed_df = df.groupby('barcode', as_index=False)['count'].agg({'sum_count': 'sum', 'counts_list': list})

collapsed_df = collapsed_df.sort_values(by='sum_count', ascending=False)
collapsed_df = collapsed_df.reset_index(drop=True)
collapsed_df.to_csv('4_collapsed_barcodes.csv', index=False)
