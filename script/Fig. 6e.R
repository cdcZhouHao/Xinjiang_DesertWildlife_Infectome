library(readxl)
library(vegan)
library(ggplot2)


# ==============================
# Read data
# ==============================

file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\28_Procrustes_Analysis\\Procrustes.xlsx"

# Sheet1 = MGE
# Sheet2 = ARGs

s1 <- as.data.frame(read_excel(file_path, sheet = 1))
s2 <- as.data.frame(read_excel(file_path, sheet = 2))


# Set sample names
rownames(s1) <- s1[,1]
rownames(s2) <- s2[,1]


# Transpose: samples as rows, features as columns
mge <- t(s1[,-1])
args <- t(s2[,-1])


# ==============================
# Sample matching
# ==============================

samples <- intersect(
  rownames(mge),
  rownames(args)
)

mge <- mge[samples, ]
args <- args[samples, ]


# ==============================
# Hellinger transformation + Bray-Curtis distance
# ==============================

dist_mge <- vegdist(
  decostand(mge, "hellinger"),
  method = "bray"
)

dist_arg <- vegdist(
  decostand(args, "hellinger"),
  method = "bray"
)



# ==============================
# PCoA
# ==============================

coords_mge <- cmdscale(
  dist_mge,
  k = 2,
  eig = TRUE
)$points


coords_arg <- cmdscale(
  dist_arg,
  k = 2,
  eig = TRUE
)$points


rownames(coords_mge) <- samples
rownames(coords_arg) <- samples



# ==============================
# Procrustes analysis
# ==============================

pro_res <- procrustes(
  coords_mge,
  coords_arg,
  symmetric = TRUE
)


# Permutation test
set.seed(123)

pres <- protest(
  coords_mge,
  coords_arg,
  permutations = 999
)



# ==============================
# Statistics
# ==============================

m2_val <- round(pres$ss, 4)
r_val  <- round(pres$t0, 3)
p_val  <- round(pres$signif, 4)


cat(
  "Procrustes statistics:\n",
  "M2 =", m2_val,
  "\nR =", r_val,
  "\nP =", p_val,
  "\n"
)



# ==============================
# Plotting data
# ==============================

df <- data.frame(
  sample = samples,
  
  # MGE
  x1 = pro_res$X[,1],
  y1 = pro_res$X[,2],
  
  # ARGs
  x2 = pro_res$Yrot[,1],
  y2 = pro_res$Yrot[,2]
)



# ==============================
# Procrustes plotting
# ==============================

p <- ggplot(df) +
  
  # Origin reference lines
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.5
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.5
  ) +
  
  
  # Sample connecting lines
  geom_segment(
    aes(
      x = x1,
      y = y1,
      xend = x2,
      yend = y2
    ),
    color = "grey70",
    alpha = 0.5,
    linewidth = 0.3
  ) +
  
  
  # MGE points
  geom_point(
    aes(
      x = x1,
      y = y1,
      color = "MGE",
      shape = "MGE"
    ),
    size = 4
  ) +
  
  
  # ARGs points
  geom_point(
    aes(
      x = x2,
      y = y2,
      color = "ARGs",
      shape = "ARGs"
    ),
    size = 3
  ) +
  
  
  # Colors
  scale_color_manual(
    name = NULL,
    values = c(
      "MGE" = "#F89095",
      "ARGs" = "#ADC9E0"
    )
  ) +
  
  # Shapes
  scale_shape_manual(
    name = NULL,
    values = c(
      "MGE" = 16,
      "ARGs" = 17
    )
  ) +
  
  
  theme_bw() +
  
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(
      face = "bold",
      size = 14
    )
  ) +
  
  
  labs(
    title = "Procrustes Analysis",
    
    subtitle = bquote(
      M^2 == .(m2_val) ~ "," ~
        R == .(r_val) ~ "," ~
        P == .(p_val)
    ),
    
    x = "Dim 1",
    y = "Dim 2"
  )

print(p)