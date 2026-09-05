# Load necessary packages
library(tidyverse)
library(ggridges)
library(patchwork)
library(readxl)

# 1. Import and data processing
file_path <- "C:/Users/zhouhao/OneDrive/Desktop/Xinjiang_Altun_Project/14_Ridgeline_Plot/Ridgeline_Plot.xlsx"
df <- read_excel(file_path, col_names = c("Category", "Identity", "Length"))

# Sort Category according to the median of Identity
df$Category <- fct_reorder(df$Category, df$Identity, .fun = median, .desc = FALSE)

# Get sorted category levels (Key: used to lock the Y-axis)
cat_levels <- levels(df$Category)

# 2. Draw the left ridgeline plot
p1 <- ggplot(df, aes(x = Identity, y = Category, fill = after_stat(x))) +
  geom_density_ridges_gradient(
    scale = 3, 
    rel_min_height = 0.01,
    alpha = 0.7
  ) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  scale_y_discrete(limits = cat_levels, expand = expansion(add = c(0.5, 2))) +  # Adjust Y-axis alignment
  scale_fill_gradientn(
    colors = c("#6761A0", "#87C3A3", "#FFF5B4", "#FF9355", "#B82D42")
  ) +
  theme_ridges(grid = FALSE) +
  labs(x = "Identity", y = "Family") +
  theme(
    legend.position = "none",
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    panel.grid.major.y = element_line(color = "grey90")
  )

# 3. Draw the right box plot
p2 <- ggplot(df, aes(x = Length, y = Category)) +
  geom_jitter(width = 0.1, alpha = 0.2, size = 0.3, color = "darkblue") + 
  geom_boxplot(fill = "grey90", color = "black", outlier.shape = NA, alpha = 0.3) +
  scale_y_discrete(limits = cat_levels, expand = expansion(add = c(0.5, 2.22))) +
  coord_cartesian(xlim = c(0, 3000)) + 
  theme_minimal() +
  labs(x = "Length", y = NULL) +
  theme(
    axis.text.y = element_blank(),
    panel.grid = element_blank(),             # Remove grid
    axis.line.x = element_line(color = "black", size = 0.5), # Add X-axis line
    axis.line.y = element_line(color = "black", size = 0.5)  # Add Y-axis line
  )

# 4. Final combination (Set width proportions)
combined_plot <- p1 + p2 + plot_layout(widths = c(2, 2))

# Output
print(combined_plot)