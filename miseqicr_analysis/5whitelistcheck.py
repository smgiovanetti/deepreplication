import pandas as pd

barcodes_revcomp_df = pd.read_csv('4_barcodes_revcomp.csv')
final_white_origin_df = pd.read_csv('final_white_origin.csv')

barcodes_revcomp_df['whitelist'] = barcodes_revcomp_df['reverse_complement'].isin(final_white_origin_df['barcode'])
barcodes_revcomp_df['whitelist'] = barcodes_revcomp_df['whitelist'].map({True: 'whitelist', False: 'absent'})

merged_df = pd.merge(barcodes_revcomp_df, final_white_origin_df, left_on='reverse_complement', right_on='barcode', how='left')

if 'counts_list' in merged_df:
    merged_df.drop(columns=['counts_list'], inplace=True)

merged_df.to_csv('4_crossreference_whiteblacklist.csv', index=False)
