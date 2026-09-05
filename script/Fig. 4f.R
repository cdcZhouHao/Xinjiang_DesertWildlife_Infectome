# ✅ Load required libraries
library(ggplot2)
library(dplyr)
library(readxl)

# ✅ 1. Import data (please confirm file path)
data <- read_excel("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\17_Zoonotic_Risk\\total_predictions_filter.xlsx") 

# ✅ 2. Reverse sort annotation
annotation_levels <- rev(unique(data$annotation))
data <- data %>%
  mutate(annotation = factor(annotation, levels = annotation_levels)) %>%
  arrange(annotation, category, sample)

# ✅ 3. Update species color mapping
# Color definitions for 6 species
category_colors <- c(
  "Bos mutus" = "#FFED6F", 
  "Equus kiang" = "#FB8072", 
  "Gazella subgutturosa" = "#80B1D3", 
  "Equus ferus przewalskii" = "#66C2A5",
  "Equus hemionus hemionus" = "#F781BF", 
  "Grus nigricollis" = "#BC80BD"  
)

# ✅ 4. Data preprocessing
data <- data %>% mutate(sample = as.character(sample))

# ✅ 5. Insert blank rows for each annotation
last_rows <- data %>%
  group_by(annotation) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  mutate(
    sample = paste0("blank_", row_number()),
    value = NA, lower = NA, upper = NA,
    group = NA, category = NA, SequenceID = NA # Key modification: disease changed to SequenceID
  )

# ✅ 6. Merge data and set sample order
data <- bind_rows(data, last_rows) %>%
  arrange(annotation, category, sample) %>%
  mutate(sample = factor(sample, levels = unique(sample)))

data_clean <- data %>% filter(!grepl("^blank_", sample))

# ✅ 7. Calculate positions
annotation_labels <- data_clean %>%
  mutate(annotation = factor(annotation, levels = annotation_levels)) %>%
  group_by(annotation) %>%
  summarize(
    y_pos = mean(as.numeric(sample)),
    x_pos = 1.05,
    label = first(annotation)
  ) %>% ungroup()

function_blocks <- data_clean %>%
  mutate(annotation = factor(annotation, levels = annotation_levels)) %>%
  group_by(annotation) %>%
  summarize(
    ymin = min(as.numeric(sample)) - 0.4,
    ymax = max(as.numeric(sample)) + 0.4,
    xmin = 0,
    xmax = 1.02
  )

# ✅ 8. Plotting (mark pentagram only when SequenceID has content)
ggplot(data, aes(x = value, y = sample, color = group)) +
  # Left color blocks
  geom_tile(aes(x = -0.05, y = sample, fill = category),
            width = 0.06, height = 0.6, color = "white") +
  scale_fill_manual(values = category_colors, na.value = NA) +
  
  # [Core Modification] Mark only rows where SequenceID is not empty
  # Use filter(!is.na(SequenceID)) to filter out empty rows
  ggstar::geom_star(
    data = data %>% filter(!is.na(SequenceID)), 
    aes(x = -0.15, y = as.numeric(sample)),
    starshape = 1,       # Star shape 1 = standard pentagram
    fill = "#DE2910",    # Five-star red flag color
    color = "#DE2910",   # Border color
    size = 2,            # Size
    na.rm = TRUE
  ) +
  
  # Gray boxes separating annotations
  geom_rect(data = function_blocks,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            inherit.aes = FALSE,
            fill = NA, color = "#BEBEBE", linetype = "solid", size = 0.1) +
  
  # Error bars and center points
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0, linewidth = 0.65) +
  geom_point(size = 1) +
  
  # [New] Add a dashed line at X = 0.293
  geom_vline(xintercept = 0.293, 
             linetype = "dashed",    # Dashed line type
             color = "#878787",        # Line color
             size = 0.5) +          # Line thickness
  
  # Annotation label text
  geom_text(data = annotation_labels,
            aes(x = x_pos, y = y_pos, label = label),
            hjust = 0.1, size = 2.5, color = "black") +
  
  # Group color mapping
  scale_color_manual(values = c(
    "Low" = "#98C597",
    "Medium" = "#9B9AFB",
    "High" = "#FE1F1F"
  )) +
  
  # Coordinate settings
  xlim(-0.15, 1.2) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    plot.margin = margin(10, 30, 10, 50)
  ) +
  labs(x = "Value", y = "Virus sequence", color = "Group") +
  theme(legend.position = "none")