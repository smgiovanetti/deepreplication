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
nicrbc <- read.csvead.csv('~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/4-12-24stackedR.csv')
proportion.correct <- (1 - apply(matrix(nicrbc$replicate_2, nrow = 9), 1, function (x) sum(x[3:5])/sum(x[1:5]))[c(6, 2, 7, 5, 1, 9, 3, 4, 8)])
proportion.primdim <- (apply(matrix(nicrbc$replicate_2, nrow = 9), 1, function (x) x[4]/sum(x[1:5]))[c(6, 2, 7, 5, 1, 9, 3, 4, 8)])
plot(rep(proportion.correct[c(2, 3, 1)], each = 9) * rep(proportion.correct[c(5, 6, 4)], each = 3) * proportion.correct[c(8, 9, 7)], 
     ylim = c(0, 1),
     ylab = "Predicted fraction completely correct")
predicted.nicr.classifications <- cbind(correct = rep(proportion.correct[c(2, 3, 1)], each = 9) * rep(proportion.correct[c(5, 6, 4)], each = 3) * proportion.correct[c(8, 9, 7)],
                                        one.wrong = (1 - rep(proportion.correct[c(2, 3, 1)], each = 9)) * rep(proportion.correct[c(5, 6, 4)], each = 3) * proportion.correct[c(8, 9, 7)] +
                                          rep(proportion.correct[c(2, 3, 1)], each = 9) * (1 - rep(proportion.correct[c(5, 6, 4)], each = 3)) * proportion.correct[c(8, 9, 7)] +
                                          rep(proportion.correct[c(2, 3, 1)], each = 9) * rep(proportion.correct[c(5, 6, 4)], each = 3) * (1 - proportion.correct[c(8, 9, 7)]),
                                        two.wrong = (1 - rep(proportion.correct[c(2, 3, 1)], each = 9)) * (1 - rep(proportion.correct[c(5, 6, 4)], each = 3)) * proportion.correct[c(8, 9, 7)] +
                                          rep(proportion.correct[c(2, 3, 1)], each = 9) * (1 - rep(proportion.correct[c(5, 6, 4)], each = 3)) * (1 - proportion.correct[c(8, 9, 7)]) +
                                          (1 - rep(proportion.correct[c(2, 3, 1)], each = 9)) * rep(proportion.correct[c(5, 6, 4)], each = 3) * (1 - proportion.correct[c(8, 9, 7)]),
                                        all.wrong = (1 - rep(proportion.correct[c(2, 3, 1)], each = 9)) * (1 - rep(proportion.correct[c(5, 6, 4)], each = 3)) * (1 - proportion.correct[c(8, 9, 7)]))
 
 
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
