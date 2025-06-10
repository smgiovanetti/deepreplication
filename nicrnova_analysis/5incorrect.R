library(tidyverse)
library(Biostrings)

#output_file.txt is consolidated frequency table for mms experiments
df <- read.table("output_file.txt", header = FALSE, col.names = c("HC1", "BC1", "HC2", "BC2", "HC3", "BC3", "frequency", "classification", "time", "mms"))

#whitelist of barcodes we deteremined to be correctly associated with cargo (correct sequence)
df_white <- read.csv("4-15-24longthreshWHITE.csv")

#merged_incorrect.csv is consolidated 'incorrect' barcodes from py classification steps, aggregate in case any repeat barcodes
incorrect <- read.csv("merged_incorrect.csv")

# add reverse complement of barcode to barcode lists
reverse_complement <- function(seq) {
  rev_seq <- reverseComplement(DNAString(seq))
  return(as.character(rev_seq))
}

df_white$reverse_complement <- sapply(df_white$barcode, reverse_complement)
incorrect$reverse_complement <- sapply(incorrect$barcode, reverse_complement)

#aggregate in case duplicate barcodes present
incorrect <- incorrect %>%
  group_by(barcode) %>%
  summarize(
    classification_x = paste(classification_x, collapse = ", "),
    type = unique(type)[1],
    whitelist = paste(whitelist, collapse = ", "),
    classification_y = paste(classification_y, collapse = ", "),
    replicate = paste(replicate, collapse = ", "),
    reverse_complement = paste(reverse_complement, collapse = ", ")
  )

correct <- df_white %>%
  group_by(barcode) %>%
  summarize(
    classification_x = paste(classification_x, collapse = ", "),
    type = paste(type, collapse = ", "),
    whitelist = paste(whitelist, collapse = ", "),
    classification_y = paste(classification_y, collapse = ", "),
    replicate = paste(replicate, collapse = ", "),
    reverse_complement = paste(reverse_complement, collapse = ", ")
  )

# classify as correct or incorrect (inefficient)
incorrect_BC2 <- df %>%
  filter(BC2 %in% incorrect$reverse_complement)
incorrect_BC2 <- incorrect_BC2 %>%
  left_join(incorrect %>% select(reverse_complement, type), by = c("BC2" = "reverse_complement"))

collapsed_BC2_incorrect <- incorrect_BC2 %>%
  group_by(HC1, BC1, HC2, BC2, HC3, BC3, classification, time, mms, type) %>%
  summarise(frequency = sum(frequency)) %>%
  ungroup()

time0_BC2_incorrect <- collapsed_BC2_incorrect %>%
  filter(time == 0) %>%
  select(HC1, BC1, HC2, BC2, HC3, BC3, classification, frequency, type)

correct_BC2 <-df %>%
  filter(BC2 %in% whitelist_agg$reverse_complement)

collapsed_BC2_correct <- correct_BC2 %>%
  group_by(HC1, BC1, HC2, BC2, HC3, BC3, classification, time, mms) %>%
  summarise(frequency = sum(frequency)) %>%
  ungroup()

collapsed_BC2_correct_rep <- left_join(collapsed_BC2_correct, whitelist_agg, by = c("BC2" = "reverse_complement"))
collapsed_BC2_correct_rep <- collapsed_BC2_correct_rep %>%
  select(-barcode, -classification_x, -type, -whitelist, -classification_y)

time0_BC2_correct <- collapsed_BC2_correct_rep %>%
  filter(time == 0, replicate == 2) %>%
  select(HC1, BC1, HC2, BC2, HC3, BC3, classification, frequency)
time0_BC2_correct <- time0_BC2_correct %>%
  mutate(type = "correct")

BC2_summary_correct <- time0_BC2_correct %>%
  distinct(HC2, BC2, type)
BC2_summary_correct <- BC2_summary_correct %>%
  group_by(HC2, type) %>%
  summarize(unique_row_count = n(), .groups = 'drop')

BC2_summary_incorrect <- time0_BC2_incorrect %>%
  distinct(HC2, BC2, type)
BC2_summary_incorrect <- BC2_summary_incorrect %>%
  group_by(HC2, type) %>%
  summarize(unique_row_count = n(), .groups = 'drop')

BC2_summary_combined <- bind_rows(BC2_summary_correct, BC2_summary_incorrect)
BC2_summary_combined$type <- factor(BC2_summary_combined$type, levels = c("misassociation", "primer", "snp", "correct"))

HC2_mapping <- c("ATAC" = "rad10", "ACAG" = "rad10neg", "AATG" = "ercc1")

BC2_summary_combined <- BC2_summary_combined %>%
  mutate(HC2 = case_when(
    HC2 %in% names(HC2_mapping) ~ HC2_mapping[HC2],
    TRUE ~ HC2  
  ))
colors <- c("correct" ="#94D1BF", "snp" = "#9F2F7FFF", "primer"="#CD4071FF", "misassociation" = "#F1605DFF")

ggplot(BC2_summary_combined, aes(x = HC2, y = unique_row_count, fill = type)) +
  geom_bar(stat = "identity") +
  labs(x = "HC2", y = "unique bc2", title = "barcode distribution b2 r2") +
  scale_fill_manual(values = colors) + 
  theme_minimal() +
  ylim(0, 2200) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
