# Divergent patterns of richness and density in the global soil seed bank

Code by Emma Ladouceur & Alistair Auffret

### Data
*Auffret, Alistair G., Emma Ladouceur, Natalie S. Haussmann, Eirini Daouti, Tatiana G. Elumeeva, Ineta Kačergytė, Jonas Knape, et al. 2024. “ A Global Database of Soil Seed Bank Richness, Density, and Abundance.” Ecology 105(11). https://doi.org/10.1002/ecy.4438*

* All you need is **sb_pub.csv** Downloaded from above

* OTHER ENTITIES: **model objects** Each linear mixed effects model used in this analyses is saved as a model object *(.RData)* so you can just load them to recreate figures, rather than run them on your local machine.

### R Scripts

* **1_Data_Prep.R** Quantifying metrics, transforming data, reporting on data for supplementary Table 1. Division of soil seed bank observation into ecosystems. Tables S1. Summary of the Global Seed Bank Database. 

* **2_Figure_1.R** Figure 1. The global record of the soil seed bank. 

* **3_Figure_2.R**  Figure 2: Scale-dependent richness in soil seed banks across ecosystems. 

* **4_Figure_3.R**  Figure 3: Density of seeds m-2 in the soil seed bank across ecosystems. Tables S4. Table S4. Summary of model estimates of seed density-2. Table S6. Average soil seed bank densities per realm. 

* **5_Figure_4** Figure 4: The joint relationship between predicted species richness m-2 and seed density m-2 in the soil seed bank. Table S5. Predicted estimates of diversity at different scales. 

* **6_Figure_S1** Figure S1. Posterior Predictive Checks of all models.

* **7_Table_S3.r** Table S3. Richness-area slopes. 

* **8_Figure_S2** Figure_S2. Species richness in the soil seed bank as a function of total soil samples (m2) for every biome and ecoregion. 

* **cluster** All statistical models were run on a HPC Cluster. Submit scripts and R scripts for each statistical model found in this folder and the models saved as .RData files. Model objects can be found within the data repository.
