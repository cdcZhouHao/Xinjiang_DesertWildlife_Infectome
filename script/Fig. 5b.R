library(tidyverse)
library(readxl)
library(ggplot2)
library(patchwork)

# Read data
data <- read_xlsx("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\25_Pathogenic_Bacteria_Phylogenetic_Tree\\statistic.xlsx", sheet = "Sheet1")
colnames(data)[1] <- "Species"

# 1. Process data
df_binary <- data %>% 
  select(Species, Metaphlan, BMG) %>%
  pivot_longer(-Species, names_to = "Type", values_to = "Value") %>%
  mutate(Value = as.factor(Value))

df_cat <- data %>%
  select(Species, `E. kiang`, `E. f. przewalskii`, `E. h. hemionus`, `B. mutus`, `G. subgutturosa`, `G. nigricollis`) %>%
  pivot_longer(-Species, names_to = "Type", values_to = "Value") %>%
  mutate(Value = as.factor(Value))

df_num <- data %>%
  select(Species, rplA, rplC, rpoB, gyrB, ftsY, groEL, nusG) %>%
  pivot_longer(-Species, names_to = "Type", values_to = "Value") %>%
  mutate(Value = as.factor(Value))

# 2. Plotting function
plot_matrix <- function(df, hide_y_axis = FALSE, custom_colors = NULL) {
  p <- ggplot(df, aes(x = Type, y = factor(Species, levels = rev(data$Species)))) +
    geom_tile(aes(fill = Value), color = "white") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 9),
      axis.title = element_blank(),
      legend.position = "right"
    )
  
  if(hide_y_axis) {
    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  }
  
  if(is.null(custom_colors)) {
    p <- p + scale_fill_brewer(palette = "Set3", name = "Category", na.value = "white")
  } else {
    p <- p + scale_fill_manual(values = custom_colors, name = "Category", na.value = "white")
  }
  return(p)
}

# 3. Draw subplots and set custom colors
p1 <- plot_matrix(df_binary, hide_y_axis = FALSE, custom_colors = c("0" = "white", "1" = "#A6CEE3"))
p2 <- plot_matrix(df_cat, hide_y_axis = TRUE, custom_colors = c("a" = "#FA9FB5", "b" = "#B2DF8A"))

colors_p3 <- c(
  "1" = "#EFEDF5",
  "2" = "#BCBDDC",
  "4" = "#807DBA",
  "5" = "#54278F"
)
p3 <- plot_matrix(df_num, hide_y_axis = TRUE, custom_colors = colors_p3)

# 4. Combine and save
p_combined <- p1 + p2 + p3 + 
  plot_layout(ncol = 3, widths = c(0.8, 2.4, 2.8), guides = "collect") & 
  theme(legend.position = "right")

print(p_combined)