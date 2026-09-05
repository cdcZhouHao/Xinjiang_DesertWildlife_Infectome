# 1. Load necessary libraries
library(vegan)
library(ggplot2)
library(dplyr)

# 2. Read data (please ensure the path is correct)
# Note: Backslashes in the file path need to be double backslashes \\ or single forward slashes / in R
file_path <- "C:/Users/zhouhao/OneDrive/Desktop/Xinjiang_Altun_Project/04_Bacterial_Diversity/merged_abundance_table_species.sorted.tsv"
df <- read.table(file_path, header = TRUE, sep = "\t", check.names = FALSE)

# 3. Data preprocessing
# Set species names as row names, transpose matrix (vegan requirement: rows are samples, columns are species)
rownames(df) <- df$clade_name
df_counts <- t(df[, -1])

# 4. Calculate Alpha diversity indices
shannon <- diversity(df_counts, index = "shannon")
richness <- specnumber(df_counts) # Observed species
simpson <- diversity(df_counts, index = "simpson")

# Integrate results
alpha_result <- data.frame(
  SampleID = rownames(df_counts),
  Shannon = shannon,
  Richness = richness,
  Simpson = simpson
)

# 5. Prepare grouping information
# Manually specify groups based on your sample IDs
metadata <- data.frame(
  SampleID = c("EFP1","EFP2","EFP3","EFP4","EK1","EK2","EK3","EK4",
               "EHH1","EHH2","EHH3","EHH4","BM1","BM2","BM3","BM4",
               "GS1","GS2","GS3","GS4","GN1"),
  Group = c(rep("EFP",4), rep("EK",4), rep("EHH",4), rep("BM",4), rep("GS",4), "GN")
)

# Merge data
plot_data <- merge(alpha_result, metadata, by = "SampleID")

# [Key Step] Set group order: prevent ggplot from sorting alphabetically (e.g., putting BM first)
group_order <- c("EFP", "EK", "EHH", "BM", "GS", "GN")
plot_data$Group <- factor(plot_data$Group, levels = group_order)

# 6. [Manually specify colors]
# You can modify these HEX color codes according to your preference
my_colors <- c(
  "EFP" = "#66C2A5", # Blue tone
  "EK"  = "#FB8072", # Green tone
  "EHH" = "#F781BF", # Red tone
  "BM"  = "#FFED6F", # Purple tone
  "GS"  = "#80B1D3", # Orange tone
  "GN"  = "#BC80BD"  # Gray tone
)


################################################################################

# Plotting
p <- ggplot(plot_data, aes(x = Group, y = Shannon, fill = Group)) +
  # Adjust boxplot; if there are not enough samples, it will automatically show only the median line
  geom_boxplot(outlier.shape = NA, alpha = 0.4, width = 0.5, color = "black") +
  # Key: use geom_point instead of geom_jitter, or reduce jitter width
  # This way the single-sample group GN1 will land precisely in the middle of the tick mark
  geom_jitter(width = 0.1, size = 3, alpha = 0.99, shape = 21, color = "black") +
  scale_fill_manual(values = my_colors) +
  theme_classic() +
  labs(
    title = "Bacterial Alpha Diversity (Shannon)",
    subtitle = "Note: GN group contains only one replicate", # Add subtitle note
    x = "Species",
    y = "Shannon Index"
  ) +
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # --- Key code for adding borders below ---
    # 1. Add a rectangular box to the entire plotting panel
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.1),
    # 2. Ensure axis tick marks are outside the box (optional)
    axis.line = element_blank() 
  )

print(p)


################################################################################
# Overall difference test (Kruskal-Wallis Test)
# Perform Kruskal-Wallis test
# Assuming you want to check group differences for the Shannon index
kw_test <- kruskal.test(Shannon ~ Group, data = plot_data)

# View results
print(kw_test)
# If p-value < 0.05, it indicates significant differences between at least two groups


################################################################################
# Pairwise comparisons between groups (Wilcoxon Test / Dunn Test)
# Install and load package
# install.packages("ggpubr")
library(ggpubr)

# Define groups you want to compare (excluding single-sample group GN)
comparisons <- list( 
  c("EHH", "GS"), 
  c("EHH", "BM"),
  c("BM", "EK")
)

# 1. Plot and display your image
final_plot <- p + stat_compare_means(comparisons = comparisons, 
                                     method = "wilcox.test", 
                                     label = "p.signif")
print(final_plot)