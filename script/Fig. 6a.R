library(dplyr)
library(readxl)
library(ggraph)
library(igraph)
library(tidygraph)
library(RColorBrewer)
library(magrittr)

# ------------------------------------------------------------------
# 1. Read and prepare data
# ------------------------------------------------------------------
input_file <- "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\24_ARGs_Annotation_v3\\ARGs_abundance_annotation.xlsx" 

# Please modify the sheet index according to your actual situation (e.g., write 1 for the first sheet)
df <- read_excel(input_file, sheet = 3) 

# Check if column names match, ensuring type, subtype, sum_tpm are included
# If column names differ, uncomment the line below and modify to match your actual column names:
# colnames(df) <- c("type", "subtype", "sum_tpm")

# ------------------------------------------------------------------
# 2. Build node data
# ------------------------------------------------------------------
# Subnodes (Subtype)
children_df <- df %>%
  mutate(
    name = subtype,
    size = sum_tpm,
    parent = type,
    fill_group = type,
    shortname = subtype
  ) %>%
  select(name, size, parent, fill_group, shortname)

# Level-one nodes (Type)
parents_df <- df %>%
  group_by(type) %>%
  summarise(size = sum(sum_tpm), .groups = "drop") %>%
  mutate(
    name = type,
    parent = "root",
    fill_group = type,
    shortname = type
  ) %>%
  select(name, size, parent, fill_group, shortname)

# Root node (size set to the total sum of all major categories to avoid circlepack layout scale calculation errors)
root_node <- data.frame(
  name = "root",
  size = sum(parents_df$size),
  parent = NA_character_,
  fill_group = "root",
  shortname = "root"
)

# Combine nodes and create edges
vertices <- bind_rows(root_node, parents_df, children_df) %>%
  mutate(id = 1:n())

edges <- vertices %>%
  filter(!is.na(parent)) %>%
  select(from = parent, to = name)

# ------------------------------------------------------------------
# 3. Build graph object
# ------------------------------------------------------------------
d <- graph_from_data_frame(edges, vertices = vertices)

# ------------------------------------------------------------------
# 4. Set color palette
# ------------------------------------------------------------------
parents_for_color <- unique(parents_df$name)
# RColorBrewer's Set3 supports up to 12 colors; safely handle it here
n_cols <- min(max(length(parents_for_color), 3), 12)
base_cols <- brewer.pal(n_cols, "Set3")
parent_cols <- colorRampPalette(base_cols)(length(parents_for_color))
names(parent_cols) <- parents_for_color

# ------------------------------------------------------------------
# 5. Plotting
# ------------------------------------------------------------------
p <- ggraph(d, layout = "circlepack", weight = size) +
  # Draw subcategory circles (depth == 2)
  geom_node_circle(
    data = . %>% filter(depth == 2),
    aes(fill = fill_group),
    color = "grey30",
    alpha = 0.9,
    linewidth = 0.2
  ) +
  # Draw major category circles (depth == 1)
  geom_node_circle(
    data = . %>% filter(depth == 1),
    aes(color = fill_group),
    fill = NA,
    linewidth = 0.5
  ) +
  # Add major category labels
  geom_node_text(
    data = . %>% filter(depth == 1),
    aes(label = shortname),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  # Add subcategory labels (Note: adjust the threshold 50000 here according to your actual TPM values)
  geom_node_text(
    data = . %>% filter(depth == 2 & size >= 50000), 
    aes(label = shortname),
    size = 2,
    color = "black",
    check_overlap = TRUE
  ) +
  scale_fill_manual(values = parent_cols, na.value = "white") +
  scale_color_manual(values = parent_cols, na.value = "transparent") +
  theme_void() +
  theme(legend.position = "none") +
  coord_fixed()

# Preview the plot in RStudio
print(p)