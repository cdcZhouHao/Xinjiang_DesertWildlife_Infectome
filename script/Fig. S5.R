library(tidyverse)
library(reshape2)
library(ggsci)
library(patchwork) # Must install this package: install.packages("patchwork")

# --- 1. Plotting function ---
plot_taxa <- function(file_path, title_name) {
  df <- read.table(file_path, header=T, sep="\t", check.names=F, quote="")
  data_long <- melt(df, id.vars = "clade_name", variable.name = "Sample", value.name = "Abundance")
  
  top10 <- data_long %>%
    group_by(clade_name) %>%
    summarise(mean_abun = mean(Abundance)) %>%
    arrange(desc(mean_abun)) %>%
    slice(1:10) %>%
    pull(clade_name)
  
  data_plot <- data_long %>%
    mutate(Taxa = if_else(clade_name %in% top10, clade_name, "Others")) %>%
    group_by(Sample, Taxa) %>%
    summarise(Abundance = sum(Abundance), .groups = 'drop') %>%
    mutate(Group = str_extract(Sample, "[A-Za-z]+"))
  
  data_plot$Sample <- factor(data_plot$Sample, levels = unique(data_long$Sample))
  data_plot$Group <- factor(data_plot$Group, levels = unique(data_plot$Group))
  
  p <- ggplot(data_plot, aes(x = Sample, y = Abundance, fill = Taxa)) +
    geom_bar(stat = "identity", position = "stack", width = 0.8, alpha = 0.8) +
    facet_grid(~Group, scales = "free_x", space = "free_x") + 
    scale_fill_d3("category20") + 
    scale_y_continuous(expand = c(0, 1), limits = c(0, 100.1)) +
    theme_bw() +
    labs(y = "Abundance (%)", x = "", title = title_name) +
    # --- Core modification: Shrink legend and add legend title ---
    guides(fill = guide_legend(
      title = title_name,      # Add corresponding title to the legend (e.g., Phylum, Class)
      ncol = 1,                # Force legend to be arranged in a single column
      keywidth = unit(0.3, "cm"),  # Shrink width of color boxes
      keyheight = unit(0.3, "cm"), # Shrink height of color boxes
      label.theme = element_text(size = 6) # Shrink legend text font size
    )) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
      plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
      legend.title = element_text(size = 8, face = "bold"), # Legend title font size
      legend.key.size = unit(0.3, "cm"),
      panel.spacing = unit(0.1, "lines"),
      strip.text = element_text(size = 7)
    )
  return(p)
}

# --- 2. Batch generate plots ---
base_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\03_Bacterial_Abundance\\"

p1 <- plot_taxa(paste0(base_path, "merged_abundance_table_phylum.sorted.tsv"), "Phylum")
p2 <- plot_taxa(paste0(base_path, "merged_abundance_table_class.sorted.tsv"), "Class")
p3 <- plot_taxa(paste0(base_path, "merged_abundance_table_order.sorted.tsv"), "Order")
p4 <- plot_taxa(paste0(base_path, "merged_abundance_table_family.sorted.tsv"), "Family")
p5 <- plot_taxa(paste0(base_path, "merged_abundance_table_genus.sorted.tsv"), "Genus")
p6 <- plot_taxa(paste0(base_path, "merged_abundance_table_species.sorted.tsv"), "Species")

# --- 3. Patchwork arrangement ---
# Note: Do not use guides = "collect" inside plot_layout
# Because once collected, all legends will merge, making it impossible to assign individual titles like Phylum/Class to each legend
final_plot <- (p1 | p2) / (p3 | p4) / (p5 | p6)