library(UpSetR)

# ==========================
# 1. Read data
# ==========================

file_path <- "C://Users//zhouhao//OneDrive//Desktop//Xinjiang_Altun_Project//19_Shared_Viruses_UpSet//merged_TPM_replace_sum_replace.tsv"

df <- read.delim(
  file_path,
  header = TRUE,
  sep = "\t",
  check.names = FALSE
)


# ==========================
# 2. Extract abundance matrix
# ==========================

rownames(df) <- df$Contig

df_matrix <- df[, -1]


# Convert to numeric type
df_matrix <- as.data.frame(
  lapply(df_matrix, function(x) as.numeric(as.character(x)))
)

rownames(df_matrix) <- rownames(df)



# ==========================
# 3. Convert to Presence / Absence
# ==========================

df_binary <- as.data.frame(
  lapply(df_matrix, function(x){
    ifelse(x > 0, 1, 0)
  })
)

rownames(df_binary) <- rownames(df_matrix)



# ==========================
# 4. Check data
# ==========================

print(dim(df_binary))

print(colSums(df_binary))



# ==========================
# 5. Set species colors
# ==========================

species_colors <- c(
  GN  = "#BC80BD",
  GS  = "#80B1D3",
  BM  = "#FFED6F",
  EK  = "#FB8072",
  EFP = "#66C2A5",
  EHH = "#F781BF"
)



# ==========================
# 6. Set sets order
# ==========================

sets_order <- c(
  "EHH",
  "EFP",
  "BM",
  "EK",
  "GS",
  "GN"
)

# ==========================
# 7. Display UpSet plot in window
# ==========================

upset(
  
  df_binary,
  
  sets = sets_order,
  
  nsets = length(sets_order),
  
  keep.order = TRUE,
  
  order.by = "freq",
  
  
  sets.bar.color = species_colors[sets_order],
  
  main.bar.color = "black",
  
  matrix.color = "black",
  
  
  point.size = 3,
  
  line.size = 0.5,
  
  
  number.angles = 0,
  
  
  text.scale = c(
    1.5,  #shared
    1.3,  #25000
    1.3,  #number of viral seq
    1.2,  #30000
    1.2,  #GN/GS...
    1.0   #80468
  ),
  
  
  mainbar.y.label = "Shared Viral Sequences",
  
  sets.x.label = "Number of Viral Sequences"
  
)