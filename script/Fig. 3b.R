############################################################
# Equus virus family correlation heatmap
# Add Virus Class annotation dots on both X and Y axes
############################################################


#############################
# 1. Load R packages
#############################

library(readxl)
library(tidyverse)
library(reshape2)
library(factoextra)
library(grid)
library(cowplot)
library(ggplotify)
library(Hmisc)
library(writexl)



#############################
# 2. Read Excel TPM matrix
#############################

file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\31_Equus_Virus_Correlation\\Equus_virus_TPM.xlsx"


tpm <- read_excel(
  file_path,
  sheet = 1
)

tpm <- as.data.frame(tpm)



#############################
# 3. Set virus family names as row names
#############################

rownames(tpm) <- tpm[,1]

tpm <- tpm[,-1]



#############################
# 4. Extract EFP, EHH, EK samples
#############################

equus <- tpm %>%
  select(
    matches("^EFP"),
    matches("^EHH"),
    matches("^EK")
  )


cat(
  "Extracted sample count:",
  ncol(equus),
  "\n"
)



#############################
# 5. Remove virus families with total abundance of 0
#############################

equus_filtered <- equus[
  rowSums(equus) > 0,
]


cat(
  "Virus families remaining after total abundance filter:",
  nrow(equus_filtered),
  "\n"
)



#############################
# 6. Remove virus families with prevalence < 50%
#############################

prevalence <- rowSums(equus_filtered > 0) /
  ncol(equus_filtered)


equus_prevalence_filtered <- equus_filtered[
  prevalence >= 0.5,
]


cat(
  "Virus families remaining after 50% prevalence filter:",
  nrow(equus_prevalence_filtered),
  "\n"
)



#############################
# 6.1 Save filtered results
#############################

output_df <- equus_prevalence_filtered %>%
  rownames_to_column(
    var="Virus_Family"
  )


output_excel_path <- 
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\31_Equus_Virus_Correlation\\Filtered_Equus_virus_TPM.xlsx"


write_xlsx(
  output_df,
  output_excel_path
)



#############################
# 7. Read virus family Class annotations
#############################

class_file <- 
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\31_Equus_Virus_Correlation\\Virus_family_class.xlsx"


virus_class <- read_excel(
  class_file
)


virus_class <- as.data.frame(
  virus_class
)



#############################
# 8. Match and filter virus family Class
#############################

annotation_class <- virus_class[
  match(
    rownames(equus_prevalence_filtered),
    virus_class$Family
  ),
]


rownames(annotation_class) <-
  annotation_class$Family



# Check for missing values
cat(
  "Missing Class count:",
  sum(is.na(annotation_class$Class)),
  "\n"
)



#############################
# 9. log10 transformation
#############################

equus_log <- log10(
  equus_prevalence_filtered + 1
)



#############################
# 10. Spearman correlation analysis
#############################

equus_data <- t(equus_log)



corr_result <- rcorr(
  as.matrix(equus_data),
  type="spearman"
)



corr <- corr_result$r


p_value <- corr_result$P

#############################
# 10.1 Export correlation coefficients and p-values table
#############################

library(writexl)

# --- Method A: Export as wide format (matrix form, similar to heatmap data structure) ---
# Convert correlation coefficient and p-value matrices to data frames and move row names to a separate column
corr_wide <- as.data.frame(corr) %>% rownames_to_column(var = "Virus_Family")
pval_wide <- as.data.frame(p_value) %>% rownames_to_column(var = "Virus_Family")

# --- Method B: Export as long format (pairwise combination form, ideal for checking specific pairings) ---
corr_long <- melt(corr, varnames = c("Virus1", "Virus2"), value.name = "Spearman_Rho")
pval_long <- melt(p_value, varnames = c("Virus1", "Virus2"), value.name = "P_Value")

# Merge correlation coefficients and p-values
corr_p_combined <- merge(corr_long, pval_long, by = c("Virus1", "Virus2"))

# Add significance marker column
corr_p_combined$Significance <- ""
corr_p_combined$Significance[corr_p_combined$P_Value < 0.05] <- "*"
corr_p_combined$Significance[corr_p_combined$P_Value < 0.01] <- "**"
corr_p_combined$Significance[corr_p_combined$P_Value < 0.001] <- "***"

# Sort by absolute correlation coefficient value
corr_p_combined <- corr_p_combined[order(-abs(corr_p_combined$Spearman_Rho)), ]


# --- Write to Excel file ---
output_corr_excel <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\31_Equus_Virus_Correlation\\Equus_virus_Correlation_Results.xlsx"

write_xlsx(
  list(
    "Long_Format" = corr_p_combined,    # Long format: intuitively display correlation and P-value for each pair
    "Correlation_Matrix" = corr_wide,   # Matrix format: correlation coefficients
    "P_value_Matrix" = pval_wide        # Matrix format: P-values
  ),
  output_corr_excel
)

cat("Correlation coefficients and P-values table successfully exported to:", output_corr_excel, "\n")

#############################
# 11. Significance markers
#############################

sig <- matrix(
  "",
  nrow=nrow(p_value),
  ncol=ncol(p_value)
)


sig[p_value < 0.05] <- "*"

sig[p_value < 0.01] <- "**"

sig[p_value < 0.001] <- "***"



rownames(sig)<-rownames(p_value)
colnames(sig)<-colnames(p_value)



#############################
# 12. Clustering
#############################

hc <- hclust(
  as.dist(1-corr),
  method="average"
)


order <- hc$order



corr_cluster <- corr[
  order,
  order
]


sig_cluster <- sig[
  order,
  order
]



#############################
# 13. Order Class according to clustering sequence
#############################

annotation_class_cluster <-
  annotation_class[order,]


annotation_class_cluster$index <-
  1:nrow(annotation_class_cluster)



#############################
# 14. Convert to long format
#############################

corr_df <- melt(
  corr_cluster
)


sig_df <- melt(
  sig_cluster
)



plot_data <- merge(
  corr_df,
  sig_df,
  by=c(
    "Var1",
    "Var2"
  )
)



colnames(plot_data)<-
  c(
    "Virus1",
    "Virus2",
    "Correlation",
    "Significance"
  )



#############################
# 15. Set factor orders
#############################

virus_order <-
  rownames(corr_cluster)


plot_data$Virus1 <-
  factor(
    plot_data$Virus1,
    levels=virus_order
  )


plot_data$Virus2 <-
  factor(
    plot_data$Virus2,
    levels=virus_order
  )



#############################
# 16. Keep only the lower-right triangle
#############################

plot_data$idx1 <-
  match(
    plot_data$Virus1,
    virus_order
  )


plot_data$idx2 <-
  match(
    plot_data$Virus2,
    virus_order
  )



plot_data$Correlation[
  plot_data$idx1 < plot_data$idx2
] <- NA


plot_data$Significance[
  plot_data$idx1 < plot_data$idx2
] <- ""



#############################
# 17. Class colors
#############################

class_colors <- c(
  
  "Caudoviricetes"="#1F78B4",
  "Malgrandaviricetes"="#33A02C",
  "Pisoniviricetes"="#E31A1C",
  "Megaviricetes"="#FF7F00",
  "Duplopiviricetes"="#6A3D9A",
  "Leviviricetes"="#B15928",
  "Stelpaviricetes"="#A6CEE3",
  "Tolucaviricetes"="#FB9A99",
  "Alsuviricetes"="#CAB2D6",
  "Monjiviricetes"="#FDBF6F",
  "Faserviricetes"="#B2DF8A",
  "Polintoviricetes"="#999999"
  
)



#############################
# 18. Draw heatmap
#############################

p <- ggplot(
  plot_data,
  aes(
    Virus1,
    Virus2,
    fill=Correlation
  )
)+
  
  
  geom_tile(
    color="white"
  )+
  
  
  geom_text(
    aes(
      label=Significance
    ),
    size=3,
    color="black"
  )+
  
  
  scale_fill_gradient2(
    low="#ADC9E0",
    mid="white",
    high="#F89095",
    midpoint=0,
    limits=c(-1,1),
    na.value="transparent"
  )+
  
  
  scale_y_discrete(
    position="right"
  )+
  
  
  coord_fixed(
    ratio=1,
    clip="off"
  )+
  
  
  theme_bw()+
  
  
  theme(
    
    axis.text.x=
      element_text(
        angle=90,
        hjust=1,
        vjust=0.5,
        size=7
      ),
    
    
    axis.text.y=
      element_text(
        size=7
      ),
    
    
    panel.grid=
      element_blank(),
    
    
    panel.border=
      element_blank(),
    
    
    # Adjust top and right margins to leave space for dots above X-axis and to the right of Y-axis
    plot.margin=
      margin(
        50, 80, 5, 5
      )
    
  )+
  
  
  labs(
    
    x=NULL,
    y=NULL,
    
    fill="Spearman\nrho",
    
    caption=
      "* p < 0.05\n** p < 0.01\n*** p < 0.001"
    
  )



#############################
# 19. Add Class dots (Top of X-axis & Right of Y-axis)
#############################

p <- p +
  
  # 19.1 Right side of Y-axis dots
  geom_point(
    data=annotation_class_cluster,
    aes(
      x=length(virus_order) + 1,
      y=factor(rownames(annotation_class_cluster), levels=virus_order),
      color=Class
    ),
    size=2.6,
    inherit.aes=FALSE
  )+
  
  # 19.2 Top of X-axis dots
  geom_point(
    data=annotation_class_cluster,
    aes(
      x=factor(rownames(annotation_class_cluster), levels=virus_order),
      y=length(virus_order) + 1,
      color=Class
    ),
    size=2.6,
    inherit.aes=FALSE
  )+
  
  
  scale_color_manual(
    values=class_colors,
    name="Virus Class"
  )



print(p)