

percent_by_incorrect <- read.csv('~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/correctincorrectgraph.csv')


color_palette_incorrect <- c(
  "2" = "#51be94",
  "1" = "#38a0ba",
  "0" = "#2e358c"
)
ggplot(percent_by_incorrect, aes(x = classification, y = percentage, fill = factor(num_incorrect, levels = c("2", "1", "0")))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "genotype", y = "percent", fill = "% incorrect") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = color_palette_incorrect)

ggsave("7-25-24correctvsincorrectfullnicrblues.pdf", width = 5.9, height = 2.5)


###predicted
predicted_incorrect <- read.csv('~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/ms_predicted_correctincorrectgraph.csv')


color_palette_incorrect <- c(
  "2" = "#51be94",
  "1" = "#38a0ba",
  "0" = "#2e358c"
)
ggplot(predicted_incorrect, aes(x = classification, y = percentage, fill = factor(num_incorrect, levels = c("2", "1", "0")))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "genotype", y = "percent", fill = "% incorrect") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = color_palette_incorrect)

ggsave("2-28-25predictedcorrectvsincorrectfullnicrblues.pdf", width = 5.9, height = 2.5)
