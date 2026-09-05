# 1. Load necessary libraries
library(ggplot2)
library(vegan)

# 2. Read data
dist_mat <- read.table("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\09_Virus_Diversity\\Beta_diversity.dist-braycurtis.table", header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)
dist_obj <- as.dist(dist_mat)

group_info <- read.table("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\09_Virus_Diversity\\group.txt", header = TRUE, row.names = 1, sep = "\t")
group_info$group <- as.factor(group_info$group)

# 3. PCoA calculation
pcoa <- cmdscale(dist_obj, k = 2, eig = TRUE)
pcoa_result <- as.data.frame(pcoa$points)
colnames(pcoa_result) <- c("PCoA1", "PCoA2")

eig <- pcoa$eig
eig_percent <- round(eig/sum(eig) * 100, 2)

pcoa_result$group <- group_info[rownames(pcoa_result), "group"]

# 4. PERMANOVA test (Adonis)
adonis_res <- adonis2(dist_obj ~ group, data = group_info)
dune_adonis <- paste0("PERMANOVA R2: ", round(adonis_res$R2[1], 3), 
                     ", P-value: ", adonis_res$`Pr(>F)`[1])

# 5. Plotting (modify color mapping section to correspond with group names)
# Define the correspondence between group names and colors here
color_map <- c(
  "EFP" = "#66C2A5",
  "EK"  = "#FB8072",
  "EHH" = "#F781BF",
  "BM"  = "#FFED6F",
  "GS"  = "#80B1D3",
  "GN"  = "#BC80BD"
)

p <- ggplot(pcoa_result, aes(x = PCoA1, y = PCoA2, color = group)) +
  geom_jitter(size = 3, alpha = 0.8, width = 0.01, height = 0.01) +
  geom_hline(yintercept = 0, colour = "#BEBEBE", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "#BEBEBE", linetype = "dashed") +
  labs(x = paste0("PCoA 1 (", eig_percent[1], "%)"),
       y = paste0("PCoA 2 (", eig_percent[2], "%)"),
       caption = dune_adonis) +
  # Use the color_map defined above for precise matching
  scale_colour_manual(values = color_map) + 
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        plot.caption = element_text(hjust = 0,  size = 8))

# Output and save
print(p)
ggsave("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\09_Virus_Diversity\\PCoA.pdf", p, width = 7, height = 6)

###################################################################################

p <- ggplot(pcoa_result, aes(x = PCoA1, y = PCoA2, color = group)) +
  # 1. Add confidence ellipse (placed below scatter points)
  # level = 0.95 indicates 95% confidence interval
  # geom = "polygon" generates a filled polygon
  stat_ellipse(aes(fill = group), geom = "polygon", alpha = 0.1, color = NA) +
  
  # 2. Scatter plot
  geom_jitter(size = 3, alpha = 0.8, width = 0.01, height = 0.01) +
  
  geom_hline(yintercept = 0, colour = "#BEBEBE", linetype = "dashed") +
  geom_vline(xintercept = 0, colour = "#BEBEBE", linetype = "dashed") +
  
  labs(x = paste0("PCoA 1 (", eig_percent[1], "%)"),
       y = paste0("PCoA 2 (", eig_percent[2], "%)"),
       caption = dune_adonis) +
  
  # 3. Color mapping (note that both colour and fill need to be set simultaneously)
  scale_colour_manual(values = color_map) + 
  scale_fill_manual(values = color_map) + 
  
  theme_bw() +
  theme(legend.position = "right",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_text(color = "black", size = 10),
        plot.caption = element_text(hjust = 0, size = 8))

# Output and save
print(p)
ggsave("C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\09_Virus_Diversity\\PCoA-v2.pdf", p, width = 7, height = 6)