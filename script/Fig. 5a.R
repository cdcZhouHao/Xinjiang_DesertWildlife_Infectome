# Load necessary packages
library(tidyverse)

# 1. Read data
df <- read.delim("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\03_Bacterial_Abundance\\merged_abundance_table_phylum.sorted.tsv", check.names = FALSE)

# 2. Data processing: Convert to long format and calculate mean by group
df_long <- df %>%
  pivot_longer(cols = -clade_name, names_to = "sample", values_to = "abundance") %>%
  mutate(group = str_extract(sample, "[A-Za-z]+")) %>%
  group_by(clade_name, group) %>%
  summarise(mean_abundance = mean(abundance), .groups = 'drop')

# 3. Map group names
df_long_mapped <- df_long %>%
  mutate(group = recode(group,
                        "GS" = "Gazella subgutturosa",
                        "GN" = "Grus nigricollis",
                        "EK" = "Equus kiang",
                        "EHH" = "Equus hemionus hemionus",
                        "EFP" = "Equus ferus przewalskii",
                        "BM" = "Bos mutus"))

# 4. Filter top 10 species, group the rest as "Other"
top10_species <- df_long_mapped %>%
  group_by(clade_name) %>%
  summarise(total = sum(mean_abundance)) %>%
  slice_max(total, n = 10) %>%
  pull(clade_name)

df_processed <- df_long_mapped %>%
  mutate(clade_name = if_else(clade_name %in% top10_species, clade_name, "Other")) %>%
  group_by(group, clade_name) %>%
  summarise(mean_abundance = sum(mean_abundance), .groups = 'drop') %>%
  group_by(group) %>%
  mutate(mean_abundance = mean_abundance / sum(mean_abundance)) %>%
  ungroup()

# 5. Define color vector
my_custom_colors <- c(
  "Actinomycetota"        = "#FCCDE5",
  "Bacteroidota"          = "#8DD3C7",
  "Campylobacterota"      = "#BEBADA",
  "Elusimicrobiota"       = "#FB8072",
  "Firmicutes"            = "#80B1D3",
  "Lentisphaerota"        = "#FDB462",
  "Methanobacteriota"     = "#B3DE69",
  "Pseudomonadota"        = "#FFFFB3",
  "Spirochaetota"         = "#D9D9D9",
  "Verrucomicrobiota"     = "#BC80BD",
  "Other"                 = "#999999"
)

# 6. Plotting (modified region for vertical display)
p <- ggplot(df_processed, aes(x = group, y = mean_abundance, fill = clade_name)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
  scale_fill_manual(values = my_custom_colors) +
  
  # --- Use scale_x_discrete to modify X-axis label text ---
  scale_x_discrete(
    labels = c(
      "Bos mutus" = "B. mutus",                     # Abbreviation
      "Equus ferus przewalskii" = "E. f. przewalskii",     # Custom replacement text
      "Equus hemionus hemionus" = "E. h. hemionus", 
      "Equus kiang" = "E. kiang",
      "Grus nigricollis" = "G. nigricollis",
      "Gazella subgutturosa" = "G. subgutturosa"
    )
  ) +
  
  theme_classic() +
  labs(x = "Sample Group", y = "Relative Abundance", fill = "Phylum") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.1),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )