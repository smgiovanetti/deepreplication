import os
import pandas as pd

directory = 'simone/4-15-24nicrnova'

info_file = 'simone/4-15-24nicrnova/file_decode.csv'
info_df = pd.read_csv(info_file)

dfs = []

for filename in os.listdir(directory):
    if filename.endswith("_classified.csv"):
        file_number = filename.split('_')[0]
        
        df = pd.read_csv(os.path.join(directory, filename))
        
        time_condition = info_df[info_df['file'] == int(file_number)]
        df['time'] = time_condition['time'].values[0]
        df['condition'] = time_condition['condition'].values[0]
        
        dfs.append(df)

result_df = pd.concat(dfs, ignore_index=True)

output_file = os.path.join(directory, 'collated_classified_with_info.csv')
result_df.to_csv(output_file, index=False)
