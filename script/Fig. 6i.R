############################################################
# MGE - ARG - VFG Correlation Network
# Only MGE-ARG and MGE-VFG correlations
# Spearman correlation based network
############################################################


# ==========================
# 1. Load R packages
# ==========================

library(readxl)
library(dplyr)
library(Hmisc)
library(igraph)
library(ggraph)
library(ggplot2)



# ==========================
# 2. Read Excel data
# ==========================


file <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\30_MGE_ARG_VFG_Correlation\\network\\MGE-ARG-VFG.xlsx"


# Sheet1 = MGE
# Sheet2 = ARG
# Sheet3 = VFG


MGE <- read_excel(file, sheet = 1) |> 
  as.data.frame()


ARG <- read_excel(file, sheet = 2) |> 
  as.data.frame()


VFG <- read_excel(file, sheet = 3) |> 
  as.data.frame()



# ==========================
# 3. Set gene names as rownames
# ==========================


rownames(MGE) <- MGE$subtype
MGE$subtype <- NULL


rownames(ARG) <- ARG$subtype
ARG$subtype <- NULL


rownames(VFG) <- VFG$subtype
VFG$subtype <- NULL



# ==========================
# 4. Transpose data
# ==========================

# Rows = Samples
# Columns = Genes


MGE_t <- as.data.frame(t(MGE))

ARG_t <- as.data.frame(t(ARG))

VFG_t <- as.data.frame(t(VFG))



# Check data dimensions

print(dim(MGE_t))
print(dim(ARG_t))
print(dim(VFG_t))



# ==========================
# 5. Correlation calculation function
# ==========================


cor_network <- function(data1, data2, type1, type2){
  
  
  # Combine two data matrices
  
  combine <- cbind(data1, data2)
  
  
  # Spearman correlation analysis
  
  result <- rcorr(
    as.matrix(combine),
    type = "spearman"
  )
  
  
  # Number of genes in the first and second classes
  
  n1 <- ncol(data1)
  n2 <- ncol(data2)
  
  
  # Extract correlation coefficients
  
  cor_mat <- result$r[
    1:n1,
    (n1+1):(n1+n2)
  ]
  
  
  # Extract p-values
  
  p_mat <- result$P[
    1:n1,
    (n1+1):(n1+n2)
  ]
  
  
  # Convert to edge format
  
  edge <- expand.grid(
    node1 = rownames(cor_mat),
    node2 = colnames(cor_mat)
  )
  
  
  edge$r <- as.vector(cor_mat)
  
  edge$p <- as.vector(p_mat)
  
  
  # Filter significant correlations
  
  edge <- edge %>%
    filter(
      !is.na(r),
      !is.na(p),
      abs(r) >= 0.7,
      p < 0.01
    )
  
  
  # Add categories
  
  edge$type1 <- type1
  
  edge$type2 <- type2
  
  
  return(edge)
  
}




# ==========================
# 6. Calculate correlations
# ==========================


# --------------------------
# MGE - ARG
# --------------------------

MGE_ARG <- cor_network(
  MGE_t,
  ARG_t,
  "MGE",
  "ARG"
)



# --------------------------
# MGE - VFG
# --------------------------

MGE_VFG <- cor_network(
  MGE_t,
  VFG_t,
  "MGE",
  "VFG"
)



# ==========================
# Combine edges
# Only include MGE-ARG and MGE-VFG
# ==========================


edges <- rbind(
  MGE_ARG,
  MGE_VFG
)



# View correlation counts

print(table(edges$type1, edges$type2))



# ==========================
# Save edge information
# ==========================


write.csv(
  edges,
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\30_MGE_ARG_VFG_Correlation\\network\\MGE_ARG_MGE_VFG_correlation_edges.csv",
  row.names = FALSE
)



# ==========================
# 7. Construct node table
# ==========================


nodes <- data.frame(
  name = unique(
    c(
      edges$node1,
      edges$node2
    )
  )
)



# Determine node type

nodes$type <- ifelse(
  nodes$name %in% colnames(MGE_t),
  "MGE",
  ifelse(
    nodes$name %in% colnames(ARG_t),
    "ARG",
    "VFG"
  )
)



# ==========================
# 8. Build igraph network
# ==========================


g <- graph_from_data_frame(
  edges,
  vertices = nodes,
  directed = FALSE
)



# Node degree

V(g)$degree <- degree(g)



# ==========================
# 8.1 Export node information
# ==========================


node_info <- data.frame(
  gene = V(g)$name,
  type = V(g)$type,
  degree = V(g)$degree
)



# Sort by degree

node_info <- node_info %>%
  arrange(desc(degree))



write.csv(
  node_info,
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\30_MGE_ARG_VFG_Correlation\\network\\MGE_ARG_MGE_VFG_node_information.csv",
  row.names = FALSE
)



# View hub nodes

head(node_info,20)








# ==========================
# 9. Network plotting
# Degree-graded node sizes
# ==========================


set.seed(2026)



# ==========================
# Node degree grouping
# ==========================


V(g)$degree_group <- cut(
  V(g)$degree,
  breaks = c(0,5,10,20,30,Inf),
  labels = c(
    "1-5",
    "6-10",
    "11-20",
    "21-30",
    ">30"
  ),
  include.lowest = TRUE
)



# View degree distribution

table(V(g)$degree_group)

# ==========================
# 9. Network plotting
# Degree-graded node sizes
# ==========================


set.seed(2026)



# ==========================
# Add degree grouping attribute
# ==========================


g <- set_vertex_attr(
  g,
  "degree_group",
  value = cut(
    V(g)$degree,
    breaks = c(0,5,10,20,30,Inf),
    labels = c(
      "1-5",
      "6-10",
      "11-20",
      "21-30",
      ">30"
    ),
    include.lowest = TRUE
  )
)



# View degree grouping situation

print(
  table(
    V(g)$degree_group
  )
)



# ==========================
# Network plotting
# ==========================


p <- ggraph(
  g,
  layout = "fr"
)+
  
  
  # ==========================
  # Edges
  # ==========================

geom_edge_link(
  aes(
    width = abs(r),
    color = r
  ),
  alpha = 0.5
)+
  
  
  # Edge width
  
  scale_edge_width_continuous(
    range = c(0.2,0.6),
    name = "|Spearman r|"
  )+
  
  
  # Positive and negative correlation colors
  
  scale_edge_color_gradient2(
    low = "#377EB8",
    mid = "grey80",
    high = "#E41A1C",
    midpoint = 0,
    name = "Spearman r"
  )+
  
  
  
  # ==========================
  # Nodes
  # ==========================

geom_node_point(
  aes(
    size = degree_group,
    color = type
  )
)+
  
  
  
  # ==========================
  # Node labels
  # ==========================

geom_node_text(
  aes(
    label = name
  ),
  repel = TRUE,
  size = 3,
  max.overlaps = Inf
)+
  
  
  
  # ==========================
  # Node colors
  # ==========================

scale_color_manual(
  values = c(
    MGE = "#FEB24C",
    ARG = "#90C82B",
    VFG = "#A48AD3"
  ),
  name = "Category"
)+
  
  
  
  # ==========================
  # Node sizes
  # ==========================

scale_size_manual(
  values = c(
    "1-5" = 5,
    "6-10" = 5.5,
    "11-20" = 6,
    "21-30" = 6.5,
    ">30" = 7
  ),
  name = "Degree"
)+
  
  
  
  theme_void()



# ==========================
# Display network
# ==========================

p

ggsave(
  filename = "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\30_MGE_ARG_VFG_Correlation\\network\\MGE_ARG_VFG_network.svg",
  plot = p,
  width = 12,
  height = 8,
  units = "in"
)