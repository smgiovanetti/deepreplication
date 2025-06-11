library(tidyverse)

log_data <- read_csv("logOD.csv")

log_data <- log_data %>%
  rename(MMS = 1) %>% 
  pivot_longer(
    cols = -MMS,
    names_to = "time",
    values_to = "log_cell_number"
  ) %>%
  mutate(
    time = as.numeric(time),
    MMS = factor(MMS, levels = c("no MMS", "0.005", "0.01", "0.015", "0.02"))
  )

ggplot(log_data, aes(x = time, y = log_cell_number, color = MMS)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    x = "time (hours)",
    y = expression(log[10]~"of cell number"),
    color = "MMS concentration"
  ) +
  scale_color_manual(values = c(
    "no MMS" = "black",
    "0.005" = "#1b9e77",
    "0.01"  = "#d95f02",
    "0.015" = "#7570b3",
    "0.02"  = "#e7298a"
  ))
