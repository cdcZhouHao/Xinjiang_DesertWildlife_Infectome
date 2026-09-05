############################################################
# 1. Load R packages
############################################################

library(readxl)
library(ggplot2)
library(gggenes)
library(grid)


############################################################
# 2. Import Excel file
# ############################################################

# Modify to your Excel file path
file_path <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\06_Coronavirus\\Gene structure.xlsx"

data <- read_excel(file_path)


############################################################
# 3. Data format organization
# ############################################################

# View data
head(data)

# Ensure start and end are numeric
data$start <- as.numeric(data$start)
data$end   <- as.numeric(data$end)

# Ensure gene and molecule are character
data$gene <- as.character(data$gene)
data$molecule <- as.character(data$molecule)


############################################################
# 4. Set plotting order
# ############################################################

# Ref displayed at the top, X displayed at the bottom

data$molecule <- factor(
  data$molecule,
  levels = c("Ref", "X")
)


############################################################
# 5. Separate reference genome and sample contigs
# ############################################################

ref_data <- subset(
  data,
  molecule == "Ref"
)

sample_x <- subset(
  data,
  molecule == "X"
)


############################################################
# 6. Draw viral genome structure plot
# ############################################################

p <- ggplot() +
  
  ##########################################################
  # Reference genome
  ##########################################################

geom_gene_arrow(
  data = ref_data,
  aes(
    xmin = start,
    xmax = end,
    y = molecule,
    fill = gene
  ),
  arrowhead_width = unit(2, "mm"),
  arrowhead_height = unit(4, "mm")
) +
  
  
  ##########################################################
  # Sample contigs
  ##########################################################

geom_gene_arrow(
  data = sample_x,
  aes(
    xmin = start,
    xmax = end,
    y = molecule,
    fill = gene
  ),
  arrowhead_width = unit(0, "mm"),
  arrowhead_height = unit(0, "mm")
) +
  
  
  ##########################################################
  # Reference gene labels
  ##########################################################

geom_gene_label(
  data = ref_data,
  aes(
    xmin = start,
    xmax = end,
    y = molecule,
    label = gene
  ),
  align = "centre",       # Changed to center alignment, or "left"
  min.size = 2,           # Allow smaller font size to display (default is usually 3 or 4, making it smaller displays more)
  size = 3,
  color = "black",        # Font color
  fontface = "bold"       # Bold
) +
  
  
  ##########################################################
  # Color settings
  ##########################################################

scale_fill_manual(
  values = c(
    
    # reference genes
    "ORF1a" = "#79C6DD",
    "ORF1b" = "#C394EF",
    "S"     = "#F96FA1",
    "NS2a"  = "#D9D9D9",
    "NS5a"  = "#D9D9D9",
    "E"     = "#F39B7F",
    "M"     = "#B3DE6E",
    "N"     = "#FAB4AE",
    "HE"    = "#FFED6F",
    
    # contigs
    "contig1"  = "#D9D9D9",
    "contig2"  = "#D9D9D9",
    "contig3"  = "#D9D9D9",
    "contig4"  = "#D9D9D9",
    "contig5"  = "#D9D9D9",
    "contig6"  = "#D9D9D9",
    "contig7"  = "#D9D9D9",
    "contig8"  = "#D9D9D9",
    "contig9"  = "#D9D9D9",
    "contig10" = "#D9D9D9",
    "contig11" = "#D9D9D9",
    "contig12" = "#D9D9D9"
  )
) +
  
  
  ##########################################################
  # X-axis settings
  ##########################################################

scale_x_continuous(
  expand = c(0,0),
  n.breaks = 10
) +
  
  
  ##########################################################
  # gggenes theme
  ##########################################################

theme_genes() +
  
  theme(
    
    # Remove legend
    legend.position = "none",
    
    # X-axis line
    axis.line.x = element_line(
      color="black",
      linewidth=0.5
    ),
    
    # X-axis ticks
    axis.ticks.x = element_line(
      color="black"
    ),
    
    # Adjust font
    axis.text = element_text(
      size=10
    ),
    
    # Adjust margins
    plot.margin = margin(
      5,5,5,5
    )
  )


############################################################
# 7. Display image
# ############################################################

print(p)