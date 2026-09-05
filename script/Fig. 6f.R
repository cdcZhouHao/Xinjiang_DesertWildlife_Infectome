# ============================================================
# 安装包（如果尚未安装）
# install.packages(c("readxl", "tidyverse", "ggpubr"))
# ============================================================

library(readxl)
library(tidyverse)
library(ggpubr)


# ============================================================
# 1. 导入数据
# ============================================================

file_path <- "C:\\Users\\zhouhao\\OneDrive\\桌面\\新疆阿尔金山课题\\30_MGE和耐药毒力基因的相关性\\MGE_ARG.xlsx"

df_mge <- read_excel(
  file_path,
  sheet = "Sheet1"
)

df_arg <- read_excel(
  file_path,
  sheet = "Sheet2"
)


# ============================================================
# 2. 计算每个样本 MGE 和 ARG 总 TPM
# ============================================================

mge_sum <- df_mge %>%
  mutate(
    MGE_Total = rowSums(
      select(., -sample),
      na.rm = TRUE
    )
  ) %>%
  select(sample, MGE_Total)


arg_sum <- df_arg %>%
  mutate(
    ARG_Total = rowSums(
      select(., -sample),
      na.rm = TRUE
    )
  ) %>%
  select(sample, ARG_Total)



# ============================================================
# 3. 合并数据
# ============================================================

df_combined <- inner_join(
  mge_sum,
  arg_sum,
  by = "sample"
)


# 查看结果
print(df_combined)



# ============================================================
# 4. 绘制 MGE-ARG 相关性散点图
# ============================================================

p <- ggplot(
  df_combined,
  aes(
    x = log10(ARG_Total + 1),
    y = log10(MGE_Total + 1)
  )
) +
  
  # ----------------------------
# 散点
# ----------------------------
geom_point(
  size = 3.5,
  color = "#377EB8",     # 点颜色（蓝色）
  alpha = 0.8
) +
  
  
  # ----------------------------
# 线性回归
# ----------------------------
geom_smooth(
  method = "lm",
  linewidth = 1.2,
  color = "#E41A1C",     # 回归线颜色（红色）
  fill = "#FBB4AE"       # 置信区间颜色
) +
  
  
  # ----------------------------
# Spearman相关性
# ----------------------------
stat_cor(
  method = "spearman",
  cor.coef.name = "rho",
  label.x = min(log10(df_combined$ARG_Total + 1)),
  label.y = max(log10(df_combined$MGE_Total + 1)),
  size = 5
) +
  
  
  # ----------------------------
# 坐标轴和标题
# ----------------------------
labs(
  x = "Log10(Total ARGs TPM + 1)",
  y = "Log10(Total MGEs TPM + 1)",
  title = "Correlation between ARGs and MGEs Abundance"
) +
  
  
  # ----------------------------
# 主题
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
