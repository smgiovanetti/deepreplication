import pandas as pd

input_file = 'collated_classified_with_info.csv'
df = pd.read_csv(input_file)

df = df.drop('sequence', axis=1)

output_file = 'collated_nosequence.csv'
df.to_csv(output_file, index=False)
