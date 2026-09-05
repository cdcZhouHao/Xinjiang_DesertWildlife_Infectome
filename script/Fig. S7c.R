# =========================================================
# 1. Data preparation (reading, long formatting, Log transformation)
# =========================================================
library(ggplot2)
library(dplyr)
library(tidyr)
library(dunn.test)
library(multcompView)
library(readxl)

df_raw <- read_excel("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\24_ARGs_Annotation_v3\\ARGs_abundance_annotation.xlsx", sheet=7)

df_long <- df_raw %>%
  pivot_longer(cols = -type, names_to = "Sample", values_to = "Value") %>%
  rename(Type = type) %>%
  mutate(LgValue = log10(Value + 1))

# =========================================================
# 2. Statistical analysis
# =========================================================
kw_res <- kruskal.test(LgValue ~ Type, data = df_long)
dt_res <- dunn.test(df_long$LgValue, df_long$Type, method = "bh")

# =========================================================
# 3. Generate CLD letters (fill in the missing code part)
# =========================================================
groups <- unique(df_long$Type)
k <- length(groups)
p_matrix <- matrix(1, nrow = k, ncol = k)
rownames(p_matrix) <- groups
colnames(p_matrix) <- groups

comp <- dt_res$comparisons
pvals <- dt_res$P.adjusted

for (i in seq_along(comp)) {
  pair <- strsplit(comp[i], " - ")[[1]]
  g1 <- pair[1]
  g2 <- pair[2]
  p_matrix[g1, g2] <- pvals[i]
  p_matrix[g2, g1] <- pvals[i]
}

cld <- multcompLetters(p_matrix)
labels_df <- data.frame(
  Type = names(cld$Letters),
  Letter = cld$Letters
)

# Calculate label positions
label_pos <- df_long %>%
  group_by(Type) %>%
  summarise(y = max(LgValue) + max(df_long$LgValue) * 0.05)

labels_df <- merge(labels_df, label_pos, by = "Type")

# =========================================================
# 4. Plotting
# =========================================================
# =========================================================
# 4. Plotting (with custom colors)
# =========================================================
p1 <- ggplot(df_long, aes(x = Type, y = LgValue, fill = Type)) +
  geom_boxplot(outlier.size = 0.5, alpha = 0.7) +
  # Add custom fill colors
  scale_fill_manual(values = c(
    "MLS"                         = "#A6CEE3",
    "aminoglycoside"              = "#1F78B4",
    "bacitracin"                  = "#B2DF8A",
    "beta_lactam"                 = "#33A02C",
    "bleomycin"                   = "#FB9A99",
    "chloramphenicol"             = "#E31A1C",
    "fosfomycin"                  = "#FDBF6F",
    "multidrug"                   = "#FF7F00",
    "mupirocin"                   = "#CAB2D6",
    "other_peptide_antibiotics" = "#6A3D9A",
    "polymyxin"                   = "#FFFF99",
    "tetracycline"                = "#B15928",
    "trimethoprim"                = "#8DD3C7",
    "vancomycin"                  = "#BEBADA"
  )) +
  geom_text(data = labels_df, aes(x = Type, y = y, label = Letter),
            inherit.aes = FALSE, size = 4) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + # Keep "none" if you do not want a legend
  labs(title = "Abundance of ARGs (Log10(RPKM+1) transformed)",
       y = "Log10(RPKM + 1)",
       x = "Antibiotic Type")

print(p1)