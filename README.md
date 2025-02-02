## Repository Structure

Below is an overview of the repository’s folder structure:

```
├── datasets
│   └── csv                # Lists all crop species with their taxonomic and identifier information.
│       ├── bolsistas_incts.csv                # Lists all crop species with their taxonomic and identifier information.
│       ├── gbif_data.csv          # Defines categorical groupings (e.g., native vs. exotic) for the species.
│       ├── gbif_macrophyte.csv          # Provides basic metadata and descriptive details for each crop.
│       ├── incts.csv          # The merged dataset combining crop, species, and pollinator dependency data for analysis.
│       ├── neo_sex.csv          # Contains the pollinator dependency levels for each crop.
│       └── taoca_data.csv      # Lists the pollinator species data used in the study.
│   └── pdf                # Lists all crop species with their taxonomic and identifier information.
│       └── res_2022.pdf                # Lists all crop species with their taxonomic and identifier information.
│   └── spatial                # Lists all crop species with their taxonomic and identifier information.
│       ├── BR_Regioes_2023.shp                # Lists all crop species with their taxonomic and identifier information.
│       ├── BR_UF_2022.shp                # Lists all crop species with their taxonomic and identifier information.
│       └── brazilian_legal_amazon.shp                # Lists all crop species with their taxonomic and identifier information.
│
├── figures
│   ├── 2022_proposals.tif                # Lists all crop species with their taxonomic and identifier information.
│   ├── all_maps.tif          # Defines categorical groupings (e.g., native vs. exotic) for the species.
│   ├── map_biodiversity.tif          # Provides basic metadata and descriptive details for each crop.
│   ├── map_coords&students.tif          # The merged dataset combining crop, species, and pollinator dependency data for analysis.
│   └── proportion_taxa.tif      # Lists the pollinator species data used in the study.
│
├── figures_edit
│   ├── all_maps_edit.tif                # Lists all crop species with their taxonomic and identifier information.
│   ├── map_biodiversity_edit.tif          # Defines categorical groupings (e.g., native vs. exotic) for the species.
│   ├── map_coords&students_edit.tif          # Provides basic metadata and descriptive details for each crop.
│   └── proportion_taxa_edit.tif      # Lists the pollinator species data used in the study.
│
├── old_packages
│   ├── tabulizer_0.2.2.tar.gz          # Defines categorical groupings (e.g., native vs. exotic) for the species.
│   └── tabulizerjars_1.0.1.tar.gz           # Contains data from the Brazilian Agricultural Production (PAM) survey for 2021, detailing crop production statistics at the municipal level.
│
├── scripts
│   ├── 00. setup.R          # Defines categorical groupings (e.g., native vs. exotic) for the species.
│   ├── 01. sex survey.R          # Defines categorical groupings (e.g., native vs. exotic) for the species.
│   └── 02. data analysis.R      #Main R script for data processing, statistical analysis, and visualization.
│
├── INCT.Rproj             # RStudio project file for organizing and managing the analysis environment, ensuring reproducibility and streamlined access to scripts and data.
│
├── LICENSE             # RStudio project file for organizing and managing the analysis environment, ensuring reproducibility and streamlined access to scripts and data.
│
└── README.md              # This file
```

---

## Getting Started  

1. Clone the repository:  
   ```bash  
   git clone https://github.com/lucas-colares/INCTs.git  
   ```  
2. Open `INCT.Rproj` in RStudio.  
3. Use the provided scripts to explore and analyze the data.  

---
