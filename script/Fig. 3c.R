# ==========================================
# 0. Load necessary packages
# ==========================================
library(ape)
library(readxl)
library(sf)
library(tidyverse)
library(reshape2)
library(gridExtra)
library(MASS)      # For negative binomial regression glm.nb
library(scales)    # For axis formatting

# ==========================================
# 1. Calculate host species genetic distance
# ==========================================
tree <- read.tree("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\aaa.treefile") # Read tree file
dist_matrix <- cophenetic(tree)    # Calculate cophenetic distance (sum of branch lengths)
write.csv(as.matrix(dist_matrix), "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\genetic_distance.csv")

# ==========================================
# 2. Calculate geographic distance between sampled species
# ==========================================
df <- read_excel("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\species_location.xlsx")

# Convert to sf spatial object (coords: longitude first, latitude second, WGS84 coordinate system)
pts <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326)
geo_dist_matrix <- st_distance(pts)

# Convert to kilometers and name rows and columns
geo_dist_km <- as.matrix(geo_dist_matrix) / 1000
rownames(geo_dist_km) <- df$Species
colnames(geo_dist_km) <- df$Species

write.csv(geo_dist_km, "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\Host_Geographic_Distance_Matrix.csv")

# ==========================================
# 3. Read data and preprocessing
# ==========================================
df_geo <- read.csv("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\Host_Geographic_Distance_Matrix.csv",
                   row.names = 1, check.names = FALSE)

df_gen <- read.csv("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\genetic_distance.csv",
                   row.names = 1, check.names = FALSE)

df_virus <- read_excel("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\virus_shared_matrix.xlsx") %>%
  as.data.frame()

rownames(df_virus) <- df_virus[[1]]
df_virus <- df_virus[, -1] %>% as.matrix()

# Unify species order
species_names <- rownames(df_gen)
df_geo   <- df_geo[species_names, species_names]
df_virus <- df_virus[species_names, species_names]

# ==========================================
# 4. Convert matrices to long format and merge data
# ==========================================
melt_matrix <- function(mat, value_name) {
  mat <- as.matrix(mat)
  mat[upper.tri(mat)] <- NA
  
  df_long <- melt(mat, na.rm = TRUE)
  colnames(df_long) <- c("Host_A", "Host_B", value_name)
  
  df_long <- df_long %>% filter(Host_A != Host_B)
  return(df_long)
}

long_geo   <- melt_matrix(df_geo, "Geo_Dist_km")
long_gen   <- melt_matrix(df_gen, "Gen_Dist")
long_virus <- melt_matrix(df_virus, "Shared_Viruses")

final_data <- long_geo %>%
  left_join(long_gen, by = c("Host_A", "Host_B")) %>%
  left_join(long_virus, by = c("Host_A", "Host_B")) %>%
  mutate(
    Host_Pair = paste(Host_A, Host_B, sep = " - "),
    Shared_Viruses = as.integer(Shared_Viruses)
  )

# ==========================================
# 5. Statistical formatting and plotting functions (including correlation coefficients and custom font sizes)
# ==========================================
format_stats <- function(p, r) {
  p_str <- ifelse(p < 0.001, "P < 0.001", paste0("P = ", round(p, 3)))
  r_str <- paste0("r = ", round(r, 3))
  return(paste0(r_str, ", ", p_str))
}

plot_nb_fit_with_stats <- function(data, x_var, y_var, x_lab,
                                   line_color = "#4472C4",
                                   model_type = "nb") {
  
  # Fit negative binomial regression and extract P value
  model <- glm.nb(as.formula(paste(y_var, "~", x_var)), data = data)
  p_value <- summary(model)$coefficients[2, 4]
  
  # Calculate Spearman correlation coefficient
  r_value <- cor(data[[x_var]], data[[y_var]], method = "spearman", use = "complete.obs")
  
  # Construct prediction data
  x_range <- seq(min(data[[x_var]]), max(data[[x_var]]), length.out = 100)
  new_data <- data.frame(x_range)
  colnames(new_data) <- x_var
  
  preds <- predict(model, newdata = new_data, type = "link", se.fit = TRUE)
  
  new_data[[y_var]] <- exp(preds$fit)
  new_data$upper <- exp(preds$fit + 1.96 * preds$se.fit)
  new_data$lower <- exp(preds$fit - 1.96 * preds$se.fit)
  
  # Plotting
  ggplot() +
    geom_ribbon(
      data = new_data,
      aes(x = .data[[x_var]], ymin = lower, ymax = upper),
      fill = alpha(line_color, 0.2)
    ) +
    geom_line(
      data = new_data,
      aes(x = .data[[x_var]], y = .data[[y_var]]),
      color = line_color,
      linewidth = 1.2
    ) +
    geom_jitter(
      data = data,
      aes(x = .data[[x_var]], y = .data[[y_var]]),
      size = 2.5,
      alpha = 0.6,
      width = 0.05,
      color = line_color
    ) +
    scale_y_log10(labels = scales::comma) +
    labs(x = x_lab, y = "Shared virus count (log10)") +
    theme_classic() +
    # Uniformly adjust axis titles and text sizes here
    theme(
      axis.title = element_text(size = 14, face = "bold"), # Axis title size (modify as needed)
      axis.text  = element_text(size = 11)                # Axis tick label size
    ) +
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = format_stats(p_value, r_value),
      hjust = -0.1,
      vjust = 1.5,
      size = 6,
      fontface = "italic"
    )
}

# ==========================================
# 6. Generate charts
# ==========================================
p_b <- plot_nb_fit_with_stats(
  final_data,
  "Gen_Dist",
  "Shared_Viruses",
  "Phylogenetic distance",
  line_color = "#ADC9E0"   # Blue
)

p_c <- plot_nb_fit_with_stats(
  final_data,
  "Geo_Dist_km",
  "Shared_Viruses",
  "Geographic distance (km)",
  line_color = "#F89095"   # Red
)

# ==========================================
# 7. Save as PDF
# ==========================================
pdf("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\21_Negative_Binomial_Regression\\my_plot_with_stats.pdf", width = 8, height = 8.5)

grid.arrange(p_b, p_c, ncol = 1)

dev.off()