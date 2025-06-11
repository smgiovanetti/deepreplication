library(tidyverse)
collapsed_rep2 <- read.csv("collapsed_rep2_normlmms0e.csv")

class_count <- collapsed_rep2 %>%
  filter(time == 0, mms == 0) %>%
  count(classification)

ggplot(class_count, aes(x = classification, y = n)) +
  geom_bar(stat = "identity", fill = "#3b9bb3") +
  theme_minimal() +
  labs(x = NULL,
       y = "correct 3-NICR barcodes") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
