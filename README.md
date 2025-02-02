![alt text](https://i.imgur.com/sI3h2K5.png)
# Brazilian National Institutes of Science and Technology repository 🌴🤝🏦

## Overview

This repository archives and shares datasets, code, and resources associated with the research "*Collaborative research networks as a strategy to synthesize knowledge of Amazonian biodiversity*", by Resende et al., (2025), conducted under the INCT-SinBiAm (National Institute of Science and Technology in Synthesis of Amazonian Biodiversity) initiative. SinBiAm is a collaborative research network integrating 45 academic and non-academic institutions from Brazil and abroad, dedicated to synthesizing biodiversity data in Amazonian ecosystems. We aim to:

- Provide open access to biodiversity datasets from forest and freshwater ecosystems.
- Share analytical scripts and models used in biodiversity research and synthesis.
- Facilitate collaboration among researchers, policymakers, and educators.
- Support the training of scientists and decision-makers committed to Amazon conservation.

## Repository Structure

Below is an overview of the repository’s folder structure:

```
├── datasets      
│   └── csv      # Contains CSV data files used in the study.
│       ├── bolsistas_incts.csv      # Lists fellows affiliated with the INCTs, including identifiers and affiliations.
│       ├── gbif_data.csv      # Dataset extracted from GBIF containing species distribution data.
│       ├── gbif_macrophyte.csv      # Data on macrophyte species collected from GBIF.
│       ├── incts.csv      # Data on spatial distribution of INCTs across Brazil.
│       ├── neo_sex.csv      # Lists fellows affiliated with the INCTs and their sex.
│       └── taoca_data.csv      # Dataset extracted from TAOCA containing species data.
│   └── pdf      # Contains PDF files used in the study.
│       └── res_2022.pdf      # Document with results on the 2022 call for INCT proposals.
│   └── spatial      # Contains spatial files for geographic analysis.
│       ├── BR_Regioes_2023.shp      # Shapefile with Brazilian regions for spatial analysis.
│       ├── BR_UF_2022.shp      # Shapefile with Brazilian states for spatial analysis.
│       └── brazilian_legal_amazon.shp      # Shapefile defining the Brazilian Legal Amazon boundaries.
│
├── figures
│   ├── 2022_proposals.tif      # Barplot showing proposals submitted in 2022 by Brazilian regions.
│   ├── all_maps.tif      # Compilation of various maps used in the study.
│   ├── map_biodiversity.tif      # Map illustrating biodiversity INCTs distribution.
│   ├── map_coords&students.tif      # Map showing the distribution of coordinators and students og INCTs across Brazil.
│   └── proportion_taxa.tif      # Visualization of taxa proportions in the GBIF vs. TAOCA datasets.
│
├── figures_edit
│   ├── all_maps_edit.tif      # Edited version of all_maps.tif.
│   ├── map_biodiversity_edit.tif      # Edited version of map_biodiversity.tif.
│   ├── map_coords&students_edit.tif      # Edited version of map_coords&students.tif.
│   └── proportion_taxa_edit.tif      # Edited version of proportion_taxa.tif.
│
├── old_packages
│   ├── tabulizer_0.2.2.tar.gz      # R package for extracting tables from PDFs.
│   └── tabulizerjars_1.0.1.tar.gz      # Java dependency required for the tabulizer package.
│
├── scripts
│   ├── 00. setup.R      # Script for setting up the R environment and loading dependencies.
│   ├── 01. sex survey.R      # Script to scrap data on sex of coordinators and students of INCTs.
│   └── 02. data analysis.R      # Main R script for data processing, statistical analysis, and visualization.
│
├── INCT.Rproj      # RStudio project file for organizing and managing the analysis environment.
│
├── LICENSE      # License file specifying the terms of use and distribution of the repository.
│
└── README.md      # Documentation file providing an overview of the project and instructions for use.
```


## Getting Started  

1. Clone the repository:  
   ```bash  
   git clone https://github.com/lucas-colares/INCTs.git  
   ```  
2. Open `INCT.Rproj` in RStudio.  
3. Use the provided scripts to explore and analyze the data.  


## Citation

If you use or modify any part of this repository in your work, please cite the original paper:

> Oliveira, W., Colares, L.F., Porto, R.G., Viana, B.F., Tabarelli, M., & Lopes, A.V. (2024). *Food plants in Brazil: origin, economic value of pollination and pollinator shortage risk*. Science of The Total Environment, 169147. [doi:10.1016/j.scitotenv.2023.169147](https://doi.org/10.1016/j.scitotenv.2023.169147)


## License

This repository is released under the MIT license. You are free to use, modify, and distribute the code with proper attribution.

