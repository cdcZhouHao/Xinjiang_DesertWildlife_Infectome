library(zoo)
library(ggplot2)

# Read the first one
d1 <- read.table(
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\16_Picornaviridae_Sequencing_Depth\\Picornaviridae1_depth.txt",
  sep="\t", header=FALSE
)
colnames(d1) <- c("Contig","Position","Depth")
d1$Sample <- "Picornaviridae1"
d1$Depth_smooth <- zoo::rollmean(d1$Depth, k=100, fill="extend")

# Read the second one
d2 <- read.table(
  "C:\\Users\\zhouhao\\OneDrive\\Desktop\\Xinjiang_Altun_Project\\16_Picornaviridae_Sequencing_Depth\\Picornaviridae2_depth.txt",
  sep="\t", header=FALSE
)
colnames(d2) <- c("Contig","Position","Depth")
d2$Sample <- "Picornaviridae2"
d2$Depth_smooth <- zoo::rollmean(d2$Depth, k=100, fill="extend")

# Merge
depth_all <- rbind(d1, d2)

# Plotting
ggplot(depth_all, aes(Position, Depth_smooth, fill=Sample, color=Sample)) +
  geom_area(alpha=0.7) +
  facet_wrap(~Sample, ncol=1) +
  scale_fill_manual(values=c("Picornaviridae1"="#F781BF",
                             "Picornaviridae2"="#4DAF4A")) +
  scale_color_manual(values=c("Picornaviridae1"="#F781BF",
                              "Picornaviridae2"="#4DAF4A")) +
  theme_bw() +
  labs(x="Genome position", y="Sequencing depth") +
  theme(strip.text = element_text(size=12))