# Divergent patterns of richness and density in the global soil seed bank

Code by Emma Ladouceur & Alistair Auffret

### Data
*Auffret, Alistair G., Emma Ladouceur, Natalie S. Haussmann, Eirini Daouti, Tatiana G. Elumeeva, Ineta Kačergytė, Jonas Knape, et al. 2024. “ A Global Database of Soil Seed Bank Richness, Density, and Abundance.” Ecology 105(11). https://doi.org/10.1002/ecy.4438*

* All you need is **sb_pub.csv** Downloaded from above

* OTHER ENTITIES: **model objects** Each linear mixed effects model used in this analyses is saved as a model object *(.RData)* so you can just load them to recreate figures, rather than run them on your local machine.

### R Scripts

* **1_Data_Prep.R** Quantifying metrics, transforming data, reporting on data for supplementary Table 1, Tables S1.

* **2_Figure_1.R** Figure 1.

* **3_Figure_2.R**  Figure 2. Predict Richness values at different scales and plot values.

* **4_Figure_3.R**  Figure 3. Tables S4 & S6

* **5_Figure_4** Figure 4. Table S5.

* **6_Figure_S1** Figure S1.

* **7_Table_S3.r** Table S3.

* **8_Figure_S2** Figure_S2.

* **cluster** All statistical models were run on a HPC Cluster. Submit scripts and R scripts for each statistical model found in this folder and the models saved as .RData files. Model objects can be found within the data repository.
