# ==============================
# 1. Load packages (deduplicated & streamlined)
# ==============================
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggsci)


# ==============================
# 2. Plotting function (optimized version)
# ==============================
plot_arg_all <- function(input_file, title_name = "ARG abundance") {
  
  # Read data
  df <- read_excel(
    input_file,
    sheet = 5
  )
  
  
  # Long table conversion + group extraction
  data_plot <- df %>%
    pivot_longer(
      cols = -type,
      names_to = "Sample",
      values_to = "Abundance"
    ) %>%
    mutate(
      Group = str_extract(
        Sample,
        "[A-Za-z]+"
      ),
      
      Sample = factor(
        Sample,
        levels = unique(Sample)
      ),
      
      Group = factor(
        Group,
        levels = unique(Group)
      )
    )
  
  
  # Plotting
  p <- ggplot(
    data_plot,
    aes(
      x = Sample,
      y = Abundance,
      fill = type
    )
  ) +
    geom_col(
      position = "stack",
      width = 0.8,
      alpha = 0.8
    ) +
    
    facet_grid(
      ~Group,
      scales = "free_x",
      space = "free_x"
    ) +
    
    scale_fill_d3(
      palette = "category20"
    ) +
    
    theme_bw() +
    
    labs(
      x = NULL,
      y = "Abundance",
      title = title_name,
      fill = "ARG type"
    ) +
    
    theme(
      panel.grid = element_blank(),
      
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 6
      ),
      
      plot.title = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      
      legend.title = element_text(
        size = 8,
        face = "bold"
      ),
      
      legend.key.size = grid::unit(
        0.3,
        "cm"
      ),
      
      panel.spacing = grid::unit(
        0.3,
        "lines"
      ),
      
      strip.text = element_text(
        size = 7
      )
    )
  
  
  return(p)
}



# ==============================
# 3. Call function
# ==============================

file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\24_ARGs_Annotation_v3\\ARGs_abundance_annotation.xlsx"


# Generate ARG abundance stacked bar plot
my_plot <- plot_arg_all(
  file_path,
  "All ARG Classes"
)


# Display image
print(my_plot)