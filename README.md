# Soil seed bank richness, abundance and density across spatial scales and global biomes

Code by Emma Ladouceur & Alistair Auffret

### Data
* All you need is **gsb_db.csv**

* **GIS** folder contains a world map downloaded from the [European Commission](https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/countries) used for plotting.

* OTHER ENTITIES: **model objects** Each linear mixed effects model used in this analyses is saved as a model object *(.RData)* so you can just load them to recreate figures, rather than run them on your local machine.

### R Scripts

* **1_Data_Prep.R** Quantifying metrics, transforming data, reporting on data for supplementary tables S1 and onwards.

* **2_Figure_1.R** Mapping records for Figure 1.

* **3_Figure_2.R**  Figure 3. Predict Richness values at different scales and plot values.

* **4_Data_Prep_for_Figure_3.r**  Quantify mean fixed effect value across broad realms (Natural Terrestrial, Arable, Wetlands, Aquatic) for Figure 3

* **5_Figure_3** Figure 3. Quanitfy and plot density values.

* **6_Figure_4** Figure 4. Predict richness at 1m2 and plot together with density.

* **7_Figure_S5.r** Figure S5

* **8-11** Supplementary Tables, Figures and extras

* **cluster** All statistical models were run on a HPC Cluster. Submit scripts and R scripts for each statistical model found in this folder and the models saved as .RData files. Model objects can be found within the data repository.

