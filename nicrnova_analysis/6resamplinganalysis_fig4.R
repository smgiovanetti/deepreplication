#sample for a fixed and equal number of barcodes per classification. 
#COMPARING AGAINST ALL INCOMPLETES
library(tidyverse)
df <- read_csv("subset_BC2_2.csv") %>%
  unite(metabarcode, HC1:BC3, sep="_")
library(effsize)

number_replicates <- 500
set_metabarcodes <- c(4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)

result <- data.frame(n_reads = numeric(), number_barcodes = numeric(), replicate = numeric(), p_value = numeric(), num_significant = numeric(), num_strong_effects=numeric(), set_metabarcodes = numeric())

for (number_unique_metabarcode in set_metabarcodes) {
  for (i in 1:number_replicates) {
    sampled_metabarcodes <- df %>%
      filter(mms == 0, time == 1) %>%
      group_by(classification) %>%
      sample_n(number_unique_metabarcode) %>%
      pull(metabarcode) %>%
      unique()
    
    if(number_unique_metabarcode * 27 != length(sampled_metabarcodes)){
      browser()
    }
    
    stopifnot(number_unique_metabarcode * 27 == length(sampled_metabarcodes))
    
    first_time_point <- df %>%
      filter(time == 1, metabarcode %in% sampled_metabarcodes) %>%
      uncount(frequency) %>%
      group_by(metabarcode, classification) %>%
      count() %>%
      ungroup() %>%
      mutate(time = 1)
    
    last_time_point <- df %>%
      filter(mms == 0.015, time == 4, metabarcode %in% first_time_point$metabarcode) %>%
      uncount(frequency) %>%
      group_by(metabarcode, classification) %>%
      count() %>%
      ungroup() %>%
      mutate(time = 4)
    
    resampled <- left_join(first_time_point, last_time_point, by = join_by(metabarcode, classification), suffix = c("_first", "_last")) %>%
      mutate(normalized_reads = n_last / n_first) %>%
      drop_na(normalized_reads)
    
    
    resampled$classification<- factor(resampled$classification, ordered = FALSE)
    resampled$classification<- relevel(resampled$classification, ref = "YYY")
    
    model <- lm(normalized_reads ~ classification, data = resampled)
    
    # get the effect size, we need an estimate of the pooled standard deviation
    pooled_sd <- resampled %>%
      group_by(classification) %>%
      summarize(sd = sd(normalized_reads, na.rm = TRUE), n=n()) %>%
      ungroup %>%
      filter(n > 1) %>% # n has to be greater than 1, otherwise we cannot compute the pooled sd
      summarise(pooled_sd = sqrt(sum((n - 1) * sd^2) / sum(n - 1))) %>%
      pull
    
    incomplete_ner <- c("classificationHHN", "classificationHNH", "classificationHNN",
                        "classificationHNY", "classificationHYN", "classificationNHH",
                        "classificationNHN", "classificationNHY", "classificationNNH",
                        "classificationNNN", "classificationNNY", "classificationNYH",
                        "classificationNYN", "classificationNYY", "classificationYHN",
                        "classificationYNH", "classificationYNN", "classificationYNY",
                        "classificationYYN")
    
    p_value <- broom::tidy(model) %>%
      mutate(effect_size = estimate / pooled_sd) %>% # calculate standardized effect size by dividing the coef by the pooled standard deviation
      filter(term %in% incomplete_ner)
    
    num_significant <- sum(p_value$p.value < 0.05)
    num_strong_effects <- sum(abs(p_value$effect_size) > 0.2)
    
    result <- rbind(result,
                    data.frame(
                      n_reads_time_0 = sum(first_time_point$n),
                      n_reads_time_4 = sum(last_time_point$n),
                      number_barcodes = nrow(resampled),
                      replicate = i,
                      p_value = p_value$p.value,
                      num_significant = num_significant,
                      num_strong_effects = num_strong_effects,
                      set_metabarcodes = number_unique_metabarcode
                    )
    )
  }
}

calc_mean_ci <- function(x, n, conf = 0.95) {
  binom.test(x, n, conf.level = conf) %>% broom::tidy()
}

summary_result <- result %>%
  mutate(significant = ifelse(num_significant == 19 & num_strong_effects == 19, 1, 0)) %>%
  group_by(set_metabarcodes) %>%
  summarise(
    n_significant = sum(significant),
    percent_significant = sum(significant) / n() * 100,
    ci = list(calc_mean_ci(x = sum(significant), n=n())),
    lower_ci = ci[[1]]$conf.low * 100,
    upper_ci = ci[[1]]$conf.high * 100
  ) %>%
  select(-ci)  








summary_result %>%
  ggplot(aes(x = set_metabarcodes, y = percent_significant)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) + 
  scale_y_continuous(limits = c(0, 100), breaks = c(25, 50, 75)) +
  #geom_line() +  
  scale_x_continuous(breaks = seq(0, 150, by = 5)) +  
  labs(y = "percent YYY different than all 19 incompletes", 
       title = "500 replicates, 4 to 150 barcodes per classification, NER incomplete, 0.015") +
  theme_minimal()+
  geom_hline(yintercept = 90)
ggsave("3_14_25_500repYYYvsNER_015.pdf", width=6, height=6)





###comparing just to NNN
set_metabarcodes <- c(4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150)

result_NNN <- data.frame(n_reads = numeric(), number_barcodes = numeric(), replicate = numeric(), p_value = numeric(), num_strong_effects = numeric(), set_metabarcodes = numeric())
for (number_unique_metabarcode in set_metabarcodes) {
  for (i in 1:number_replicates) {

    sampled_metabarcodes <- df %>%
      filter(mms==0, time == 1) %>%
      group_by(classification) %>%
      sample_n(number_unique_metabarcode) %>%
      pull(metabarcode) %>%
      unique()
    
    stopifnot(number_unique_metabarcode * 27 == length(sampled_metabarcodes))
 
    first_time_point <- df %>%
      filter(time == 1, metabarcode %in% sampled_metabarcodes) %>%
      uncount(frequency) %>%
      group_by(metabarcode, classification) %>%
      count() %>%
      ungroup() %>%
      mutate(time = 0)
    
    last_time_point <- df %>%
      filter(mms == 0.015, time == 4, metabarcode %in% first_time_point$metabarcode) %>%
      uncount(frequency) %>%
      group_by(metabarcode, classification) %>%
      count() %>%
      ungroup() %>%
      mutate(time = 4)
    
    resampled <- left_join(first_time_point, last_time_point, by = join_by(metabarcode, classification), suffix = c("_first", "_last")) %>%
      mutate(normalized_reads = n_last / n_first) %>%
      drop_na(normalized_reads)
    
    resampled$classification<- factor(resampled$classification, ordered = FALSE)
    resampled$classification<- relevel(resampled$classification, ref = "YYY")
    
    model <- lm(normalized_reads ~ classification, data = resampled)
    pooled_sd <- resampled %>%
      group_by(classification) %>%
      summarize(sd = sd(normalized_reads, na.rm = TRUE), n=n()) %>%
      ungroup %>%
      filter(n > 1) %>% # n has to be greater than 1, otherwise we cannot compute the pooled sd
      summarise(pooled_sd = sqrt(sum((n - 1) * sd^2) / sum(n - 1))) %>%
      pull
    
    incomplete_NNN <- c("classificationNNN")
    
    p_value <- broom::tidy(model) %>%
      mutate(effect_size = estimate / pooled_sd) %>% # calculate standardized effect size by dividing the coef by the pooled standard deviation
      filter(term %in% incomplete_ner)
    
    num_significant <- sum(p_value$p.value < 0.05)
    num_strong_effects <- sum(abs(p_value$effect_size) > 0.2)
    
    result_NNN <- rbind(result_NNN,
                        data.frame(
                          n_reads_time_0 = sum(first_time_point$n),
                          n_reads_time_4 = sum(last_time_point$n),
                          number_barcodes = nrow(resampled),
                          replicate = i,
                          p_value = p_value$p.value,
                          num_significant = num_significant,
                          num_strong_effects = num_strong_effects,
                          set_metabarcodes = number_unique_metabarcode
                        )
    )
    
  }
}

calc_mean_ci <- function(x, n, conf = 0.95) {
  binom.test(x, n, conf.level = conf) %>% broom::tidy()
}

summary_result_NNN <- result_NNN %>%
  filter(!is.na(p_value)) %>%
  mutate(significant = ifelse(num_significant == 19 & num_strong_effects == 19, 1, 0)) %>%
  group_by(set_metabarcodes) %>%
  summarise(
    n_significant = sum(significant, na.rm = TRUE),
    total_count = n(),
    percent_significant = (sum(significant) / total_count) * 100,
    ci = list(calc_mean_ci(x = sum(significant), n=n())),
    lower_ci = ci[[1]]$conf.low * 100,
    upper_ci = ci[[1]]$conf.high * 100
  ) %>%
  select(-ci)  

summary_result_NNN %>%
  ggplot(aes(x = set_metabarcodes, y = percent_significant)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) + 
  scale_y_continuous(limits = c(0, 100), breaks = c(25, 50, 75)) +
  #geom_line() +  
  scale_x_continuous(breaks = seq(0, 150, by = 5)) +  
  labs(y = "percent YYY different than all 19 incompletes", 
       title = "500 replicates, 4 to 150 barcodes per classification, YYY vs NNN, 0.015") +
  theme_minimal()+
  geom_hline(yintercept = 90)
ggsave("3_14_25_500repYYYvsNNN_015.pdf", width=6, height=6)
