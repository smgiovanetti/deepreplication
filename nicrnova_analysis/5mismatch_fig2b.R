mismatch <- read.csv('~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/mismatched_graph.csv')

x_order <- c("rad1", "ercc4", "rad1neg", "rad10", "ercc1", "rad10neg", "rad14", "xpa", "rad14neg")
y_order <- c("rad1", "ercc4", "rad1neg", "rad10", "ercc1", "rad10neg", "rad14", "xpa", "rad14neg")

mismatch$`classified.as` <- factor(mismatch$`classified.as`, levels = x_order)
mismatch$`compared.to.seq` <- factor(mismatch$`compared.to.seq`, levels = y_order)

colors <- c("white", "#deefc8", "#8dc73f")
breaks <- c(0, 1, 4)


ggplot(mismatch, aes(x = `classified.as`, y = `compared.to.seq`, fill = `threshold_r2`)) +
  geom_tile() +
  geom_text(aes(label = ifelse(`threshold_r2` != 0, as.character(`threshold_r2`), "")), color = "black") +
  labs(x = "hardcoded barcode", y = "cargo sequence") +
  scale_fill_gradientn(colors = colors, values = scales::rescale(breaks), name = "Threshold R2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "#2e358c", color = NA),
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank()  # remove minor grid lines
  )

ggsave("~/OneDrive - National Institutes of Health/MM_MMS experiment follow ups/7-25heatmap_r2greenblue.pdf", width=6, height=3)
