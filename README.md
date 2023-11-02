# Soil seed bank richness, abundance and density across spatial scales and global biomes

# Code by Emma Ladouceur & Alistair Auffret

### Data
* All you need is **gsb_slim.csv**

* OTHER ENTITIES: **model objects** Each linear mixed effects model used in this analyses is saved as a model object *(.RData)* so you can just load them to recreate figures, rather than run them on your local machine.

### R Scripts

* **1_Data_Prep.R** Quantifying metrics, transforming data, reporting on data for supplementary tables S1 and onwards.

* **2_Figure_2.R** Mapping records for Figure 2.

* **cluster** All statistical models were run on a HPC Cluster. Submit scripts and R scripts for each model found in this folder and the models saved as .RData files. Model objects can be found within the data repository.

* **3_Figure_3_S2.R**  Figure 3 and S2

* **4_Figure_4ab_S3ab_S4.r**  Predict species richness for each Biome at two scales and plot Figure 4 a) & b), Figure S3 a) & b) & Figure S4. Also produce Table S6.

* **4_Figure_4cd.r**  Call data from **4_Figure_4ab** to produce Figure 4 c) & d) (maps)

* **5_Figure_5a.r** Figure 5a)

* **6_Figure_5b.r** Figure 5b)

* **7_Figure_S5.r** Figure S5

* **8_Figure_S6.r** Figure S6

* **9_Figure_S7.r** Figure S7

* **10_Table_S5.r** Table S5
