# Load necessary packages
library(readxl)
library(tidyverse)
library(ggrepel)

# 2. Import Excel table
df <- read_excel(
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\26_Virulence_Factors_v3/VF_abundance_annotation.xlsx",
  sheet = 6
)

# 3. Data processing
data_processed <- df %>%
  pivot_longer(
    cols = -c(type, subtype),
    names_to = "sample",
    values_to = "tpm"
  ) %>%
  group_by(type, subtype) %>%
  summarise(
    # Total number of samples (used to calculate prevalence)
    n_samples = n(),
    
    # Prevalence: proportion of samples with abundance (>0)
    prevalence = sum(tpm > 0, na.rm = TRUE) / n_samples * 100,
    
    # Average abundance
    avg_tpm = mean(tpm, na.rm = TRUE),
    
    # Log-transformed average abundance
    log_avg_tpm = log10(avg_tpm + 1),
    
    .groups = "drop"
  )
# 4. Plotting
ggplot(data_processed, aes(x = prevalence, y = log_avg_tpm, color = type)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_hline(yintercept = 4, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 80, linetype = "dashed", color = "gray50") +
  
  # Custom colors: please replace the names here with actual type names in your data
  scale_color_manual(values = c(
    "Effector delivery system"                 = "#A6CEE3",
    "Biofilm"                                   = "#1F78B4",
    "Adherence"                                 = "#B2DF8A",
    "Nutritional/Metabolic factor"              = "#33A02C",
    "Antimicrobial activity/Competitive advantage" = "#FB9A99",
    "Exotoxin"                                  = "#E31A1C",
    "Invasion"                                  = "#FDBF6F",
    "Immune modulation"                         = "#FF7F00",
    "Regulation"                                = "#CAB2D6",
    "Stress survival"                           = "#6A3D9A",
    "Motility"                                  = "#FFFF99",
    "Others"                                    = "#B15928",
    "Enzyme"                                    = "#8DD3C7",
    "translational modification"                = "#BEBADA"
  )) +
  
  geom_text_repel(
    data = data_processed %>% filter(prevalence > 80, log_avg_tpm > 4),
    aes(label = subtype), 
    size = 3, 
    max.overlaps = 20
  ) +
  
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  theme_bw() +
  labs(
    title = "Gene Prevalence vs Average RPKM (All Samples)",
    x = "Prevalence (%)",
    y = "Log10(Average RPKM + 1)",
    color = "Gene Type"
  ) +
  theme(panel.grid = element_blank())