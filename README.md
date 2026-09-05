# Xinjiang_DesertWildlife_Infectome

This repository contains data and analysis scripts for the manuscript:
> **Infectomics of Extreme Desert Wildlife Unveils Cryptic Microbial Diversity and Emerging Zoonotic Risks**  
> Hao Zhou, Yuqing Liu, Ying Li, Ji Pu, Xi Yang, Jiali Chen, Xiujuan Liu, Fulong Wang, Shan Lu, Jing Yang*, Jianguo Xu*  

---

## Directory Structure

* **`Map_data/`**: Stores the 3D satellite imagery maps of Xinjiang.
* **`Table_data/`**: Stores processed statistical tables.
* **`script/`**: Stores R scripts used for statistical analyses, model construction (NB-GLM), and figure generation.
* **`tree/`**: Stores Newick-format phylogenetic tree files and associated metadata.
---

## System Requirements & Dependencies

The analyses were performed in **R (v4.2.2)**. Key dependent packages include:
* Data manipulation & visualization: `ggplot2`, `ComplexHeatmap`, `ggraph`, `sf`, `ggridges`
* Ecology & statistics: `vegan`, `MASS` (for NB-GLM)

---

## Data Availability

* Raw sequencing data for the 21 metatranscriptomic libraries are available in the NCBI Sequence Read Archive (SRA) under BioProject accession **PRJNA1460268**.
* High-risk viral sequences have been deposited in NCBI GenBank (see manuscript Table S14 for specific accession numbers).
