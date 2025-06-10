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

list_agg <- bind_rows(correct, incorrect)
list_agg_aggregated <- list_agg %>%
  group_by(reverse_complement) %>%
  summarize(
    classification_y = paste(unique(classification_y), collapse = ", "),
    replicate = paste(unique(replicate), collapse = ", "),
    type = type[1]  # Retain the 'type' column
  ) %>%
  mutate(
    classification_y = ifelse(grepl("incorrect", classification_y), "incorrect", classification_y)
  )

# classify as correct or incorrect (inefficient)
#filter based on BC1
filtered_df_BC1 <- df %>%
  filter(BC1 %in% list_agg_aggregated$reverse_complement)

filtered_df_BC1 <- filtered_df_BC1 %>%
  left_join(
    list_agg_aggregated %>% 
      select(reverse_complement, classification_y, replicate, type),
    by = c("BC1" = "reverse_complement")
  )

filtered_df_BC1 <- filtered_df_BC1 %>%
  rename_with(
    ~ if_else(. == "classification_y", "BC1_classification", 
              if_else(. == "replicate", "BC1_replicate", 
                      if_else(. == "type", "BC1_type", .))),
    everything()
  )

#filter based on BC2
filtered_df_BC1_BC2 <- filtered_df_BC1 %>%
  filter(BC2 %in% list_agg_aggregated$reverse_complement)

filtered_df_BC1_BC2 <- filtered_df_BC1_BC2 %>%
  left_join(
    list_agg_aggregated %>% 
      select(reverse_complement, classification_y, replicate, type),
    by = c("BC2" = "reverse_complement")
  )

filtered_df_BC1_BC2 <- filtered_df_BC1_BC2 %>%
  rename_with(
    ~ if_else(. == "classification_y", "BC2_classification", 
              if_else(. == "replicate", "BC2_replicate", 
                      if_else(. == "type", "BC2_type", .))),
    everything()
  )


#filter based on BC3
filtered_df_BC1_BC2_BC3 <- filtered_df_BC1_BC2 %>%
  filter(BC3 %in% list_agg_aggregated$reverse_complement)

filtered_df_BC1_BC2_BC3 <- filtered_df_BC1_BC2_BC3 %>%
  left_join(
    list_agg_aggregated %>% 
      select(reverse_complement, classification_y, replicate, type),
    by = c("BC3" = "reverse_complement")
  )

filtered_df_BC1_BC2_BC3 <- filtered_df_BC1_BC2_BC3 %>%
  rename_with(
    ~ if_else(. == "classification_y", "BC3_classification", 
              if_else(. == "replicate", "BC3_replicate", 
                      if_else(. == "type", "BC3_type", .))),
    everything()
  )

collapsed_df <- filtered_df_BC1_BC2_BC3 %>%
  group_by(HC1, BC1, HC2, BC2, HC3, BC3, classification, time, mms) %>%
  summarise(
    frequency = sum(frequency),
    BC1_classification = unique(BC1_classification)[1],
    BC1_replicate = paste(BC1_replicate, collapse = ", "),
    BC1_type = unique(BC1_type)[1],  # Retaining BC1_type
    BC2_classification = unique(BC2_classification)[1],
    BC2_replicate = paste(BC2_replicate, collapse = ", "),
    BC2_type = unique(BC2_type)[1],  # Retaining BC2_type
    BC3_classification = unique(BC3_classification)[1],
    BC3_replicate = paste(BC3_replicate, collapse = ", "),
    BC3_type = unique(BC3_type)[1]   # Retaining BC3_type
  ) %>%
  ungroup() %>%
  mutate(
    BC1_classification = sub(",.*", "", BC1_classification),
    BC2_classification = sub(",.*", "", BC2_classification),
    BC3_classification = sub(",.*", "", BC3_classification)
  )

collapsed_df <- collapsed_df %>%
  rowwise() %>%
  mutate(
    BC1_replicate = if (all(grepl("\\b1\\b", BC1_replicate)) && !any(grepl("\\b2\\b", BC1_replicate))) {
      "1"
    } else if (all(grepl("\\b2\\b", BC1_replicate)) && !any(grepl("\\b1\\b", BC1_replicate))) {
      "2"
    } else if (any(grepl("\\b1\\b", BC1_replicate)) && any(grepl("\\b2\\b", BC1_replicate))) {
      "1,2"
    } else {
      "other"
    },
    BC2_replicate = if (all(grepl("\\b1\\b", BC2_replicate)) && !any(grepl("\\b2\\b", BC2_replicate))) {
      "1"
    } else if (all(grepl("\\b2\\b", BC2_replicate)) && !any(grepl("\\b1\\b", BC2_replicate))) {
      "2"
    } else if (any(grepl("\\b1\\b", BC2_replicate)) && any(grepl("\\b2\\b", BC2_replicate))) {
      "1,2"
    } else {
      "other"
    },
    BC3_replicate = if (all(grepl("\\b1\\b", BC3_replicate)) && !any(grepl("\\b2\\b", BC3_replicate))) {
      "1"
    } else if (all(grepl("\\b2\\b", BC3_replicate)) && !any(grepl("\\b1\\b", BC3_replicate))) {
      "2"
    } else if (any(grepl("\\b1\\b", BC3_replicate)) && any(grepl("\\b2\\b", BC3_replicate))) {
      "1,2"
    } else {
      "other"
    }
  ) %>%
  ungroup()

#looking at just time=0 barcodes
time0_barcodes <- time0_barcodes %>%
  mutate(
    num_incorrect = rowSums(across(starts_with("BC"), ~ . == "incorrect"))
  )

freq_by_incorrect <- time0_barcodes %>%
  group_by(classification, num_incorrect) %>%
  summarise(total_freq = sum(frequency), .groups = "drop")

total_freq_by_class <- time0_barcodes %>%
  group_by(classification) %>%
  summarise(total_freq = sum(frequency), .groups = "drop")

percent_by_incorrect <- freq_by_incorrect %>%
  left_join(total_freq_by_class, by = "classification") %>%
  mutate(percentage = (total_freq.x / total_freq.y) * 100) %>%
  select(classification, num_incorrect, percentage)

percent_by_incorrect <- percent_by_incorrect %>%
  mutate(
    num_incorrect = factor(num_incorrect, levels = c("0", "1", "2"))
  )

write.csv(percent_by_incorrect, file ="correctincorrectgraph.csv", row.names = FALSE)

color_palette_incorrect <- c(
  "2" = "#1f77b4",
  "1" = "#CD4071FF",
  "0" = "#94D1BF"
)
ggplot(percent_by_incorrect, aes(x = classification, y = percentage, fill = factor(num_incorrect, levels = c("2", "1", "0")))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "correct vs incorrect nicr barcodes (rep2)", x = "genotype", y = "percent", fill = "# of incorrect bc_classifications") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = color_palette_incorrect)
