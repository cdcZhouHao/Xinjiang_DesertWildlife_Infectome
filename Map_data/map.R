library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
library(ggspatial)

# 1. Load map and imagery data
tif_path <- "C:\\Users\\zhouhao\\map.tif"
shp_path <- "C:\\Users\\zhouhao\\xinjiang.geojson"

xj_raster <- terra::rast(tif_path)
xj_shp <- sf::read_sf(shp_path)

# 2. Organize sampling point data
pts_df <- data.frame(
  name = c("Bos mutus", "Equus kiang", "Gazella subgutturosa", "Equus ferus przewalskii", "Equus hemionus hemionus", "Grus nigricollis"),
  lon = c(90.525606, 90.139723, 88.941611, 88.852420, 88.836610, 90.123990),
  lat = c(37.195804, 37.410006, 45.226708, 45.335037, 45.247395, 37.416320)
)

# Convert to sf object and unify coordinate reference system
pts_sf <- st_as_sf(pts_df, coords = c("lon", "lat"), crs = 4326)
pts_sf <- st_transform(pts_sf, st_crs(xj_shp))

# ==========================================
# 2.1 Added: Calculate the center of two point groups and create a 50 km buffer
# ==========================================
# Convert to projected metric coordinate system (EPSG:32645 is suitable for local calculations in Xinjiang)
pts_projected <- st_transform(pts_sf, 32645) 

# Define two species groups
group1_names <- c("Bos mutus", "Equus kiang", "Grus nigricollis")
group2_names <- c("Gazella subgutturosa", "Equus ferus przewalskii", "Equus hemionus hemionus")

# Extract group 1, calculate its geometric center, and create a 50 km (50,000 m) buffer
center_group1 <- pts_projected |> 
  subset(name %in% group1_names) |> 
  st_union() |> 
  st_centroid()

buf_center1 <- center_group1 |> 
  st_buffer(dist = 50000) |> 
  st_transform(st_crs(xj_shp))

# Extract group 2, calculate its geometric center, and create a 50 km (50,000 m) buffer
center_group2 <- pts_projected |> 
  subset(name %in% group2_names) |> 
  st_union() |> 
  st_centroid()

buf_center2 <- center_group2 |> 
  st_buffer(dist = 50000) |> 
  st_transform(st_crs(xj_shp))


# 3. [Custom Color Configuration]
site_colors <- c(
  "Bos mutus" = "#FFED6F",  
  "Equus kiang" = "#FB8072",  
  "Gazella subgutturosa" = "#80B1D3",  
  "Equus ferus przewalskii" = "#66C2A5",
  "Equus hemionus hemionus" = "#F781BF",  
  "Grus nigricollis" = "#BC80BD"   
)

# 4. Plotting
p <- ggplot() +
  # Draw TIF 3D imagery background
  geom_spatraster_rgb(data = xj_raster) +  
  
  # Draw Xinjiang boundary outline
  geom_sf(data = xj_shp, fill = NA, color = "white", linewidth = 0.6) +
  
  # ==========================================
  # 4.1 Added: Draw the 50 km buffer polygons for both centers
  # ==========================================
  geom_sf(data = buf_center1, fill = "#E41A1C", color = "#E41A1C", alpha = 0.2, linewidth = 0.6) + # Group 1 center 50km circle
  geom_sf(data = buf_center2, fill = "#377EB8", color = "#377EB8", alpha = 0.2, linewidth = 0.6) + # Group 2 center 50km circle
  
  # Plot sampling points
  geom_sf(data = pts_sf, 
          aes(color = name), 
          shape = 16,        
          size = 3,          
          alpha = 1) +   
  
  # Apply custom colors
  scale_color_manual(values = site_colors, name = "Species") +
  
  # Add scale bar and north arrow
  annotation_north_arrow(location = "tl", which_north = "true", 
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.1) +
  
  # Set title and axes labels
  labs(x = "longitude", y = "latitude") +
  
  # Coordinate alignment and theme enhancement
  coord_sf() +  
  theme_bw() +  
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "right",
    legend.background = element_rect(fill = alpha("white", 0.8))
  )