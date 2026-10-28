# Xinjiang_DesertWildlife_Infectome

[![Status: Under Review](https://img.shields.io/badge/Status-Manuscript%20under%20review-blue.svg)](#)
[![Language](https://img.shields.io/badge/Language-R%20%7C%20Bash%20%7C%20Python-orange.svg)](#)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![NCBI BioProject](https://img.shields.io/badge/NCBI%20BioProject-PRJNA1460268-informational.svg)](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA1460268)
[![Zenodo DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.22249258-blue.svg)](https://doi.org/10.5281/zenodo.22249258)

This repository hosts reproducible analysis pipelines, statistical scripts, and processed data matrices supporting the manuscript:

> **Infectomics of Extreme Desert Wildlife Unveils Cryptic Microbial Diversity and Emerging Zoonotic Risks**  
> Hao Zhou<sup>1</sup>, Han Wang<sup>1,2,3</sup>, Yuqing Liu<sup>1,2,3</sup>, Ying Li<sup>4</sup>, Xi Yang<sup>1</sup>, Ji Pu<sup>1</sup>, Jiali Chen<sup>4</sup>, Xiujuan Liu<sup>1</sup>, Fulong Wang<sup>5</sup>, Shan Lu<sup>1</sup>, Jing Yang<sup>1,2,6,*</sup>, Jianguo Xu<sup>1,2,7,*</sup>

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
