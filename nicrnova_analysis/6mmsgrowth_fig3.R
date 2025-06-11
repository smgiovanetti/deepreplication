df <- read.table("output_file.txt", header = FALSE, col.names = c("HC1", "BC1", "HC2", "BC2", "HC3", "BC3", "frequency", "classification", "time", "mms"))
library(tidyverse)
library(Biostrings)
library(patchwork)

incorrect <- read.csv("merged_incorrect.csv")
df_white <- read.csv("4-15-24longthreshWHITE.csv")

calculate_reverse_complement <- function(seq) {
  rev_seq <- reverseComplement(DNAString(seq))
  return(as.character(rev_seq))
}

incorrect$reverse_complement <- sapply(incorrect$barcode, calculate_reverse_complement)

incorrect_agg <- incorrect %>%
  group_by(barcode) %>%
  summarize(
    classification_x = paste(classification_x, collapse = ", "),
    type = unique(type)[1],
    whitelist = paste(whitelist, collapse = ", "),
    classification_y = paste(classification_y, collapse = ", "),
    replicate = paste(replicate, collapse = ", "),
    reverse_complement = unique(reverse_complement)[1]
  )%>%
  mutate(classification_y = "incorrect")

df_white$reverse_complement <- sapply(df_white$barcode, calculate_reverse_complement)

whitelist_agg <- df_white %>%
  group_by(barcode) %>%
  summarize(
    classification_x = paste(classification_x, collapse = ", "),
    type = paste(type, collapse = ", "),
    whitelist = paste(whitelist, collapse = ", "),
    classification_y = paste(classification_y, collapse = ", "),
    replicate = paste(replicate, collapse = ", "),
    reverse_complement = unique(reverse_complement)[1]
  )

list_agg <- bind_rows(whitelist_agg, incorrect_agg)
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



collapsed_rep2 <- subset(collapsed_df, BC2_replicate == "2")
collapsed_rep2 <- collapsed_rep2 %>%
  select(-BC1_classification, -BC1_replicate, -BC1_type, -BC2_classification, -BC2_replicate, -BC2_type, -BC3_classification, -BC3_replicate, -BC3_type)

summed_frequency_2 <- aggregate(frequency ~ time + mms, data = collapsed_rep2, FUN = sum)
colnames(summed_frequency_2) <- gsub("frequency", "library_reads", colnames(summed_frequency_2))
collapsed_rep2 <- merge(collapsed_rep2, summed_frequency_2, by = c("time", "mms"), all.x = TRUE)


collapsed_rep2$timeX <- collapsed_rep2$frequency / collapsed_rep2$library_reads

mms0_rep2 <- collapsed_rep2 %>%
  filter(mms == 0) %>%
  select(-frequency, -library_reads) %>%
  rename_with(~ gsub("timeX", "mms0", .))

collapsed_rep2 <- collapsed_rep2 %>%
  left_join(mms0_rep2 %>% select(HC1, BC1, HC2, BC2, HC3, BC3, time, mms0), 
            by = c("HC1", "BC1", "HC2", "BC2", "HC3", "BC3", "time"))

collapsed_rep2$normalized_read <- collapsed_rep2$timeX / collapsed_rep2$mms0

write.csv(collapsed_rep2, "collapsed_rep2_normlmms0e.csv", row.names = FALSE)
collapsed_rep2 <- read.csv("collapsed_rep2_normlmms0e.csv")

average_rep2 <- aggregate(normalized_read ~ classification + time + mms, data = collapsed_rep2, FUN = mean)
sem_rep2 <- aggregate(normalized_read ~ classification + time + mms, data = collapsed_rep2, FUN = function(x) sd(x) / sqrt(length(x)))

average_rep2 <- merge(average_rep2, sem_rep2, by = c("classification", "time", "mms"))
colnames(average_rep2) <- c("classification", "time", "mms", "normalized_read_mean", "normalized_read_sem")


ggplot(average_rep2, aes(x = time, y = normalized_read_mean, color = classification, group = classification)) +
  geom_line() +
  geom_errorbar(aes(ymin = normalized_read_mean - normalized_read_sem, ymax = normalized_read_mean + normalized_read_sem), width = 0.2) +
  facet_wrap(~ factor(mms)) +
  labs(x = "time", y = "average read", title = "replicate 2") +
  scale_x_continuous(limits = c(0, 4), breaks = c(0,1,2,3,4)) +
  scale_y_continuous(limits = c(0, 8)) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank())

selected_classifications <- c("HHH", "HHY", "HYH", "HYY", "YHH", "YHY", "YYH", "YYY")
incomplete_classifications <- c("HHN", "HNH", "HNN", "NHH", "HYN", "HNY", "NHH", "NHY", "NHN", "NNN", "NYH", "NYN", "NNH", "NNY", "NYY", "YHN", "YNH", "YNN", "YNY", "YYN")

classification_colors <- c("HHH"= "#003f5c", 
                           "HHY"= "#58508d", 
                           "HYH" = "#8a508f", 
                           "HYY" = "#bc5090", 
                           "YHH"= "#de5a79", 
                           "YHY" ="#ff6361", 
                           "YYH" = "#ff8531", 
                           "YYY" = '#ffa600',
                           "incomplete ner complexes" = "gray")

# only the selected classifications
filtered_df <- average_rep2 %>%
  mutate(label = case_when(
    classification %in% selected_classifications ~ classification,
    classification %in% incomplete_classifications ~ "incomplete ner complexes",
    TRUE ~ NA_character_  # Exclude other classifications
  )) %>%
  filter(!is.na(label))

# plot
ggplot(filtered_df, aes(x = time, y = normalized_read_mean, color = label, group = classification)) +
  geom_line(alpha = 0.8) +
  geom_errorbar(aes(ymin = normalized_read_mean - normalized_read_sem, ymax = normalized_read_mean + normalized_read_sem), width = 0.2) +
  facet_wrap(~ factor(mms)) +
  labs(x = "time", y = "normalized read") +
  facet_wrap(~ mms, ncol = 5) +
  scale_x_continuous(breaks = c(1,2, 3, 4)) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2)) +  
  scale_color_manual(values = classification_colors) +  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave("3-14-25rep2normlmms0.pdf", width = 6, height = 6)



####5-15 plotting all replicates as separate lines
collapsed_rep2 <- collapsed_rep2 %>%
  mutate(meta_barcode = paste(HC1, BC1, HC2, BC2, HC3, BC3, sep = "_"))

collapsed_rep2_10 <- collapsed_rep2 %>%
  filter(meta_barcode %in% (
    collapsed_rep2 %>%
      filter(time == 0, frequency > 30) %>%
      pull(meta_barcode)
  ))



filtered_reps <- collapsed_rep2_10 %>%
  mutate(label = case_when(
    classification %in% selected_classifications ~ classification,
    classification %in% incomplete_classifications ~ "incomplete ner complexes",
    TRUE ~ NA_character_  
  )) %>%
  filter(!is.na(label))



ggplot(filtered_reps, aes(x = time, y = normalized_read, color = classification, group = meta_barcode)) +
  geom_line(alpha = 0.1) +
  facet_wrap(~ mms, ncol = 5) +
  labs(x = "time", y = "normalized read") +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 18, by = 2)) +
  scale_color_manual(values = classification_colors) +  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave("5-16-25allreplicates30.pdf", height = 6, width = 6)

#just XXX 
##"HHH"= "#003f5c", 
#"HHY"= "#58508d", 
#"HYH" = "#8a508f", 
#"HYY" = "#bc5090", 
#"YHH"= "#de5a79", 
#"YHY" ="#ff6361", 
#"YYH" = "#ff8531", 
#"YYY" = '#ffa600',

collapsed_rep2_10 %>%
  filter(classification == "YYY") %>%
  ggplot(aes(x = time, y = normalized_read, group = meta_barcode)) +
  geom_line(alpha = 0.05, color = "#ffa600") +  
  facet_wrap(~ mms, ncol = 5) +
  labs(x = "time", y = "normalized read", title = "yyy") +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave("5-16-25YYYreplicates30.pdf", height = 6, width = 6)

##looped across 
for (geno in names(classification_colors)) {
  
  color <- classification_colors[[geno]]
  
  p <- collapsed_rep2_10 %>%
    filter(classification == geno) %>%
    ggplot(aes(x = time, y = normalized_read, group = meta_barcode)) +
    geom_line(alpha = 0.05, color = color) +  
    facet_wrap(~ mms, ncol = 5) +
    labs(x = "time", y = "normalized read", title = geno) +
    scale_x_continuous(breaks = c(1, 2, 3, 4)) +
    scale_y_continuous(limits = c(0, 20), breaks = seq(0, 18, by = 2)) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank())
  
  ggsave(paste0("5-20-25_", geno, "_replicates30.pdf"), plot = p, height = 6, width = 6)
}


collapsed_rep2 %>%
  mutate(meta_barcode = paste(HC1, BC1, HC2, BC2, HC3, BC3, sep = "_")) %>%
  filter(classification %in% c("HHN", "HNH", "HNN", "NHH", "HYN", "HNY", "NHY", "NHN", "NNN",
                               "NYH", "NYN", "NNH", "NNY", "NYY", "YHN", "YNH", "YNN", "YNY", "YYN")) %>%
  ggplot(aes(x = time, y = normalized_read, group = meta_barcode)) +
  geom_line(alpha = 0.05, color = "gray") +  
  facet_wrap(~ mms, ncol = 5) +
  labs(x = "time", y = "normalized read") +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave("5-15-25incompletereplicates.pdf", height = 6, width = 6)

average_rep2_10 <- aggregate(normalized_read ~ classification + time + mms, data = collapsed_rep2_10, FUN = mean)
sem_rep2_10 <- aggregate(normalized_read ~ classification + time + mms, data = collapsed_rep2_10, FUN = function(x) sd(x) / sqrt(length(x)))

average_rep2_10 <- merge(average_rep2_10, sem_rep2_10, by = c("classification", "time", "mms"))
colnames(average_rep2_10) <- c("classification", "time", "mms", "normalized_read_mean", "normalized_read_sem")

filtered_df_10 <- average_rep2_10 %>%
  mutate(label = case_when(
    classification %in% selected_classifications ~ classification,
    classification %in% incomplete_classifications ~ "incomplete ner complexes",
    TRUE ~ NA_character_  # Exclude other classifications
  )) %>%
  filter(!is.na(label))

# plot
ggplot(filtered_df_10, aes(x = time, y = normalized_read_mean, color = label, group = classification)) +
  geom_line(alpha = 0.8) +
  geom_errorbar(aes(ymin = normalized_read_mean - normalized_read_sem, ymax = normalized_read_mean + normalized_read_sem), width = 0.2) +
  facet_wrap(~ factor(mms)) +
  labs(x = "time", y = "normalized read") +
  facet_wrap(~ mms, ncol = 5) +
  scale_x_continuous(breaks = c(1,2, 3, 4)) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2)) +  
  scale_color_manual(values = classification_colors) +  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())






##resuming script
###just YYY HHY
filtered_df %>%
  filter(label %in% c("YYY", "HHY", "incomplete ner complexes")) %>%
  ggplot(aes(x = time, y = normalized_read_mean, color = label, group = classification)) +
  geom_line(alpha = 0.8) +
  geom_errorbar(aes(ymin = normalized_read_mean - normalized_read_sem, ymax = normalized_read_mean + normalized_read_sem), width = 0.2) +
  facet_wrap(~ factor(mms)) +
  labs(x = "time", y = "normalized read") +
  facet_wrap(~ mms, ncol = 5) +
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 18, by = 2)) +  
  scale_color_manual(values = classification_colors) +  
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave("3-14-25rep2normlmms0_YYY_HHY_inc.pdf", width = 6, height = 6)



collapsed_rep2$mms <- factor(collapsed_rep2$mms)
collapsed_rep2$time <- factor(collapsed_rep2$time)


library(emmeans)


time_values <- c(0,1, 2, 3, 4)
mms_values <- c(0.005, 0.01, 0.015, 0.02)

results_list <- list()
for (t in time_values) {
  for (m in mms_values) {
    subset_data <- collapsed_rep2 %>%
      filter(time == t & mms == m)
    
    if (nrow(subset_data) > 0) { 
      subset_data$classification <- factor(subset_data$classification, ordered = FALSE)
      subset_data$classification <- relevel(subset_data$classification, ref = "YYY")
      
      model_lm <- lm(normalized_read ~ classification, data = subset_data)
      
      pairwise_results <- emmeans(model_lm, pairwise ~ classification, adjust = "tukey")
      
      all_pairs <- as.data.frame(pairwise_results$contrasts) %>%
        mutate(time = t, mms = m)  
      
      effect_sizes <- eff_size(pairwise_results, sigma = sigma(model_lm), edf = df.residual(model_lm)) %>% 
        as.data.frame %>% 
        mutate(contrast = gsub("[()]", "", contrast))
      
      all_pairs <- all_pairs %>%
        left_join(effect_sizes, by = "contrast")
      
      if (nrow(all_pairs) > 0) {
        results_list[[paste0("t", t, "_m", m)]] <- all_pairs
      }
    }
  }
}

final_results_t12norml <- bind_rows(results_list, .id = "group")
final_results_t12norml$p_adjusted <- p.adjust(final_results_t12norml$p.value, method = "fdr")
write.csv(final_results_t12norml, "final_results_allcomp_mm20normlALL.csv", row.names = FALSE)




significant_results <- final_results_t12norml %>%
  filter(p_adjusted < 0.05)
write.csv(significant_results, "3-15-25p_adjust_mms0norml.csv", row.names = FALSE)

significant_results <- read.csv("3-15-25p_adjust_mms0norml.csv")


ggplot(significant_results, aes(x = effect.size, y = -log(p_adjusted))) +
  geom_point(aes(color = ifelse(grepl("YYY", contrast), "YYY", "other")),
             alpha = 0.5,
             size = 1,
  ) + 
  scale_color_manual(values = c("YYY" = "purple", "other" = "black"), guide = guide_legend(title = NULL)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  labs(
    x = "effect size",
    y = "-log10(p value)",  
  ) +
  theme_minimal()
ggsave("3-15-25effectsize_normlmms0_padjusted.pdf", width = 6, height = 6)

significant_results <- significant_results %>%
  separate(contrast, into = c("contrast_1", "contrast_2"), sep = " - ")

significant_results <- significant_results %>%
  mutate(time = case_when(
    time == 1 ~ 12,
    time == 2 ~ 24,
    time == 3 ~ 36,
    time == 4 ~ 48,
    TRUE ~ time 
  ))

positive_effect_df <- significant_results %>%
  filter(effect.size > 0)

# positive_effect_df <- positive_effect_df %>%
#   rename(
#     'improved genotype' = contrast_1,
#     'less good genotype' = contrast_2 
#   )

negative_effect_df <- significant_results %>%
  filter(effect.size < 0)

negative_effect_df <- negative_effect_df %>%
  # rename(
  #   'less good genotype' = contrast_1,  
  #   'improved genotype'= contrast_2,
  # ) %>%
  mutate(
    effect.size = abs(effect.size)      
  )
write.csv(positive_effect_df, "positive_effect_mms0.csv", row.names = FALSE)
write.csv(negative_effect_df, "negative_effect_mms0.csv", row.names = FALSE)
