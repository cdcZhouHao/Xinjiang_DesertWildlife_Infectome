# ==============================================================================
# Virus Abundance Heatmap Drawing Script - Nested Grouping and Sorting Version
# ==============================================================================

# -----------------------
# 1. Load necessary packages
# -----------------------
library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(readxl)

# -----------------------
# 2. Read and process data
# -----------------------

# Read main matrix
mat_path <- "C:/Users/zhouhao/OneDrive/Desktop/Xinjiang_Altun_Project/11_Virus_Abundance_Heatmap/merged_TPM_rename_sum.xlsx"
mat_df <- read_excel(mat_path)

# Transform matrix: set the first column (Group) as row names
mat <- mat_df %>%
  column_to_rownames("Group") %>%
  as.matrix()

# Log transformation: balance extreme value differences (log10(x+1))
log_mat <- log10(mat + 1)

# Read host metadata (column annotations)
host_meta <- read_excel("C:/Users/zhouhao/OneDrive/Desktop/Xinjiang_Altun_Project/11_Virus_Abundance_Heatmap/host_metadata.xlsx")
host_meta <- host_meta[match(colnames(log_mat), host_meta$sample), ]

# Read virus metadata (row annotations)
virus_meta <- read_excel("C:/Users/zhouhao/OneDrive/Desktop/Xinjiang_Altun_Project/11_Virus_Abundance_Heatmap/virus_metadata.xlsx")
virus_meta <- virus_meta[match(rownames(log_mat), virus_meta$Family), ]

# -----------------------
# 3. Preprocessing: Set row sorting priority (Core modification)
# -----------------------
# Set Genome_type as a factor to force DNA to precede RNA
virus_meta$Genome_type <- factor(virus_meta$Genome_type, levels = c("DNA", "RNA"))

# Set Realm as a factor to ensure stable grouping logic
virus_meta$Realm <- factor(virus_meta$Realm)

# -----------------------
# 4. Custom color system
# -----------------------

# 4.1 Heatmap main body color (Blue gradient)
col_fun <- colorRamp2(
  breaks = seq(0, max(log_mat), length.out = 6),
  colors = c("white", "#EFDCE1", "#DDBDC0", "#D3A2A6", "#C5878C", "#B67073")
)

# 4.2 Host annotation colors
order_cols <- c("Artiodactyla" = "#9B9AFB", "Perissodactyla" = "#98C597", "Gruiformes" = "#EDB575")
locate_cols <- c("Altun Mountain" = "#7FB3FA", "Kalamely" = "#F5C1E0")
species_cols <- c(
  "Bos mutus" = "#FFED6F", "Equus kiang" = "#FB8072", 
  "Gazella subgutturosa" = "#80B1D3", "Equus ferus przewalskii" = "#66C2A5",
  "Equus hemionus hemionus" = "#F781BF", "Grus nigricollis" = "#BC80BD"
)

# 4.3 Virus annotation colors
realm_cols <- c(
  "Duplodnaviria" = "#8DD3C7", "Volvereviria" = "#B3DE69",
  "Riboviria" = "#FB8072", "Efunaviria" = "#FCCDE5",
  "Varidnaviria" = "#FFED6F", "Floreoviria" = "#C394EF",
  "Unassigned" = "#D9D9D9"
)
genome_cols <- c("DNA" = "#F48892", "RNA" = "#91CAE8")



# -----------------------
# 5. Build annotation objects
# -----------------------

# Top annotation
column_ha <- HeatmapAnnotation(
  Order   = host_meta$Order,
  Locate  = host_meta$Locate,
  Species = host_meta$species,
  col     = list(Order = order_cols, Locate = locate_cols, Species = species_cols),
  annotation_name_side = "left",
  show_annotation_name = TRUE,
  simple_anno_size = unit(1.8, "mm"),
  annotation_name_gp = gpar(fontsize = 7) 
)

# Left annotation
row_ha <- rowAnnotation(
  Realm  = virus_meta$Realm,
  Genome = virus_meta$Genome_type,
  col     = list(Realm = realm_cols, Genome = genome_cols),
  show_annotation_name = TRUE,
  annotation_name_rot = 90,
  simple_anno_size = unit(2, "mm"),
  annotation_name_gp = gpar(fontsize = 7)
)

# -----------------------
# 6. Draw heatmap
# -----------------------

Heatmap(
  log_mat,
  name = "log10(TPM+1)",
  col  = col_fun,
  
  top_annotation  = column_ha,
  left_annotation = row_ha,
  
  # Grouping logic
  row_split = data.frame(virus_meta$Genome_type, virus_meta$Realm),
  
  # [Key to completely removing left grouping text]
  row_title = NULL,                # Directly set to NULL to generate no grouping title text
  row_title_gp = gpar(fontsize = 0), 
  
  cluster_rows    = FALSE,          
  cluster_row_slices = FALSE,      
  cluster_columns = TRUE,          
  clustering_distance_rows = "euclidean",
  
  # Keep unit(0, "mm") if you do not want white lines between blocks
  row_gap = unit(0, "mm"),       
  
  show_row_names    = TRUE,
  row_names_side    = "right",
  row_names_gp      = gpar(fontsize = 5),
  column_names_gp   = gpar(fontsize = 8),
  column_names_rot  = 90,
  
  rect_gp = gpar(col = "white", lwd = 0.5),
  
  heatmap_legend_param = list(
    legend_width = unit(5, "cm"),
    title_position = "topcenter"
  )
)


### Save as PDF
pdf("C:/Users/zhouhao/OneDrive/Desktop/Xinjiang_Altun_Project/11_Virus_Abundance_Heatmap/virus_heatmap.pdf", width = 8, height = 10)

ht <- Heatmap(
  log_mat,
  name = "log10(TPM+1)",
  col  = col_fun,
  
  top_annotation  = column_ha,
  left_annotation = row_ha,
  
  # Grouping logic
  row_split = data.frame(virus_meta$Genome_type, virus_meta$Realm),
  
  # [Key to completely removing left grouping text]
  row_title = NULL,                # Directly set to NULL to generate no grouping title text
  row_title_gp = gpar(fontsize = 0), 
  
  cluster_rows    = FALSE,          
  cluster_row_slices = FALSE,      
  cluster_columns = TRUE,          
  clustering_distance_rows = "euclidean",
  
  # Keep unit(0, "mm") if you do not want white lines between blocks
  row_gap = unit(0, "mm"),       
  
  show_row_names    = TRUE,
  row_names_side    = "right",
  row_names_gp      = gpar(fontsize = 5),
  column_names_gp   = gpar(fontsize = 8),
  column_names_rot  = 90,
  
  rect_gp = gpar(col = "white", lwd = 0.5),
  
  heatmap_legend_param = list(
    legend_width = unit(5, "cm"),
    title_position = "topcenter"
  )
)

print(ht)
dev.off()