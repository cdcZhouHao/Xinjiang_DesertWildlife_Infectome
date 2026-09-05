# 1. Load necessary packages
library(ggplot2)
library(gghalves)
library(tidyr)
library(dplyr)
library(readxl)

# 2. Read Excel data
file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\02_1_kraken\\kraken.xlsx" 
data_raw <- read_excel(file_path, sheet = "Sheet1") 

# Ensure data is of factor type and arranged in your desired order
data_plot <- data_raw |>
  mutate(variable = factor(variable, levels = c("Bacteria", "Archaea", "Fungi", "Parasite", "Virus")))

# Define colors corresponding to the 5 categories
my_colors <- c("#8DD3C7", "#BEBADA", "#FB8072", "#B3DE69", "#FF7F00")

# 3. Plot raincloud plot
ggplot(data_plot, aes(x = variable, y = value, fill = variable, color = variable)) +
  # Cloud: half violin plot
  geom_half_violin(side = "r", position = position_nudge(x = 0.2), 
                   alpha = 0.6, color = NA) +
  
  # --- Modification: clearer and reproducible jittered points ---
  geom_jitter(
    position = position_jitter(width = 0.25, height = 0, seed = 2026), 
    size = 1, 
    alpha = 0.5
  ) +
  # -------------------------------------------------------------

  # Box: box plot
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.6, color = "black") +
  
  # Facet configuration
  facet_wrap(~variable, scales = "free", nrow = 1) +
  scale_fill_manual(values = my_colors) +
  scale_color_manual(values = my_colors) +
  
  # Theme customization
  theme_bw(base_size = 8) +
  theme(
    legend.position = "none",
    strip.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 6, color = "black") 
  ) +
  labs(x = NULL, y = "Read counts")