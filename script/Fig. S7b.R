# ==============================
# 1. Load packages
# ==============================
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggsci)


# ==============================
# 2. Plotting function
# ==============================
plot_arg_all <- function(input_file, title_name = "VF abundance") {
  
  # Read Excel 5th sheet
  df <- read_excel(
    input_file,
    sheet = 5
  )
  
  # Long data conversion + group information extraction
  data_plot <- df %>%
    pivot_longer(
      cols = -type,
      names_to = "Sample",
      values_to = "Abundance"
    ) %>%
    mutate(
      # Extract sample group information
      Group = str_extract(Sample, "[A-Za-z]+"),
      
      # Keep original sample order
      Sample = factor(
        Sample,
        levels = unique(Sample)
      ),
      
      # Keep group order
      Group = factor(
        Group,
        levels = unique(Group)
      )
    )
  
  
  # ==============================
  # Plotting
  # ==============================
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
      fill = "VF type"
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
      
      legend.text = element_text(
        size = 7
      ),
      
      legend.key.size = unit(
        0.3,
        "cm"
      ),
      
      panel.spacing = unit(
        0.3,
        "lines"
      ),
      
      strip.text = element_text(
        size = 7,
        face = "bold"
      )
    )
  
  return(p)
}



# ==============================
# 3. Input file
# ==============================
file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\26_Virulence_Factors_v3\\VF_abundance_annotation.xlsx"



# ==============================
# 4. Generate VF abundance plot
# ==============================
my_plot <- plot_arg_all(
  file_path,
  "All VF Classes"
)


# Display image
print(my_plot)