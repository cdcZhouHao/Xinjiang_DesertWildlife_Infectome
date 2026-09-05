###Pie1
# -------------------------------
# 1. Load necessary packages
# -------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(webshot2)

# -------------------------------
# 2. Read Excel data
# -------------------------------
data <- read_excel("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\13_Virus_Pie_Chart\\pie.xlsx")

# -------------------------------
# 3. Define custom color mapping
# -------------------------------
# Explicitly specify colors corresponding to the three categories
# You can modify these hex color codes as needed
color_mapping <- c(
  "Caudoviricetes" = "#D9D9D9",  
  "Unclassified"   = "#F2F2F2", 
  "Others"         = "#666666"  
)

# Apply the color mapping to the data frame to ensure consistent order
data$my_colors <- color_mapping[data$x]

# -------------------------------
# 4. Draw donut/pie chart
# -------------------------------
fig <- plot_ly(
  data,
  labels = ~x,
  values = ~y,
  type = 'pie',
  # hole = 0.4,
  textfont = list(size = 20),
  textinfo = 'label+percent',
  insidetextorientation = 'auto',
  textposition = 'auto',
  sort = FALSE,       # Maintain original sorting from Excel
  rotation = 193,     # Rotation angle set by you
  hoverinfo = 'label+value',
  # Use the matched color column from the data frame
  marker = list(
    colors = data$my_colors, 
    line = list(color = '#FFFFFF', width = 1)
  )
)

fig <- fig %>% layout(
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
)

############################################################################

###Pie2
# -------------------------------
# 1. Load necessary packages
# -------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(RColorBrewer)

# -------------------------------
# 2. Read Excel data
# -------------------------------
data <- read_excel("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\13_Virus_Pie_Chart\\pie.xlsx", sheet=2)

# -------------------------------
# 3. Automatically generate Set3 color scheme (Revised version)
# -------------------------------
n_cats <- nrow(data)

base_cols <- brewer.pal(min(max(n_cats, 3), 12), "Set3")

my_colors <- colorRampPalette(base_cols)(n_cats)

# -------------------------------
# ⭐Key fix: Convert to RGBA (add transparency)
# -------------------------------
alpha_val <- 0.7   # Transparency (0~1)

my_colors_rgba <- sapply(my_colors, function(col){
  rgb_val <- col2rgb(col)/255
  rgb(rgb_val[1], rgb_val[2], rgb_val[3], alpha = alpha_val)
})

data$my_colors <- my_colors_rgba

# -------------------------------
# 4. Draw donut/pie chart
# -------------------------------
fig <- plot_ly(
  data,
  labels = ~x,
  values = ~y,
  type = 'pie',
  # hole = 0.4,
  textfont = list(size = 12),
  textinfo = 'label+percent',
  insidetextorientation = 'auto',
  textposition = 'auto',
  sort = FALSE,
  rotation = 193,
  hoverinfo = 'label+value',
  marker = list(
    colors = data$my_colors,
    line = list(color = '#FFFFFF', width = 1)
  )
)

fig <- fig %>% layout(
  title = "Virus Families Distribution (Donut Chart)",
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
)