# ============================================================
# Install packages (if not already installed)
# install.packages(c("readxl", "tidyverse", "ggpubr"))
# ============================================================

library(readxl)
library(tidyverse)
library(ggpubr)


# ============================================================
# 1. Import data
# ============================================================

file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\30_MGE_VF_Correlation\\MGE_VFG.xlsx"

df_mge <- read_excel(
  file_path,
  sheet = "Sheet1"
)

df_vfg <- read_excel(
  file_path,
  sheet = "Sheet2"
)


# ============================================================
# 2. Calculate total TPM of MGE and VFG for each sample
# ============================================================

mge_sum <- df_mge %>%
  mutate(
    MGE_Total = rowSums(
      select(., -sample),
      na.rm = TRUE
    )
  ) %>%
  select(sample, MGE_Total)


vfg_sum <- df_vfg %>%
  mutate(
    VFG_Total = rowSums(
      select(., -sample),
      na.rm = TRUE
    )
  ) %>%
  select(sample, VFG_Total)



# ============================================================
# 3. Merge data
# ============================================================

df_combined <- inner_join(
  mge_sum,
  vfg_sum,
  by = "sample"
)


# View results
print(df_combined)



# ============================================================
# 4. Draw MGE-VFG correlation scatter plot
# ============================================================

p <- ggplot(
  df_combined,
  aes(
    x = log10(VFG_Total + 1),
    y = log10(MGE_Total + 1)
  )
) +
  
  # ----------------------------
  # Scatter points
  # ----------------------------
  geom_point(
    size = 3.5,
    color = "#377EB8",     # Point color (blue)
    alpha = 0.8
  ) +
  
  
  # ----------------------------
  # Linear regression
  # ----------------------------
  geom_smooth(
    method = "lm",
    linewidth = 1.2,
    color = "#E41A1C",     # Regression line color (red)
    fill = "#FBB4AE"       # Confidence interval color
  ) +
  
  
  # ----------------------------
  # Spearman correlation
  # ----------------------------
  stat_cor(
    method = "spearman",
    cor.coef.name = "rho",
    label.x = min(log10(df_combined$VFG_Total + 1)),
    label.y = max(log10(df_combined$MGE_Total + 1)),
    size = 5
  ) +
  
  
  # ----------------------------
  # Axes and titles
  # ----------------------------
  labs(
    x = "Log10(Total VFGs TPM + 1)",
    y = "Log10(Total MGEs TPM + 1)",
    title = "Correlation between VFGs and MGEs Abundance"
  ) +
  
  
  # ----------------------------
  # Theme
  # ----------------------------
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(
      size = 14,
      face = "bold"
    ),
    axis.text = element_text(
      size = 12
    ),
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    )
  )