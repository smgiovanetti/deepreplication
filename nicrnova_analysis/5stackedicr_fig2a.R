library(ggplot2)

df <- read.csv('~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/4-12-24stackedR.csv')

desired_order <- c("rad1", "ercc4", "rad1neg", "rad10", "ercc1", "rad10neg", "rad14", "xpa", "rad14neg")

metric_levels <- c("incorrect_misaasociation", "incorrect_primerdimer", "incorrect_SNP", "correct_withoutprim", "correct")
custom_colors <- c("#D3D5FB", "#b0abcf", "#8c85ba", "#58508D", "#58508D")  # Custom colors for each level

df$metric <- factor(df$metric, levels = metric_levels)


ggplot(df, aes(x = factor(gene, levels = desired_order), y = replicate_2, fill = metric)) +
  geom_bar(stat = "identity") +
  labs(x = "genes", y = "unique barcodes") +
  scale_fill_manual(values = custom_colors) +  
  theme_minimal() +
  ylim(0, 2100) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/7-25stacked_r2purple.pdf", width=6, height=6)



