README

Recommended workflow:

DATA PROCESSING:
	1. recode_covars.R -- Extracting covariates and recoding raw data
	2. recode_covars_subcat.R -- Recoding subcategories of some covariates 
	3. metab_explore.R -- Extracting baseline metabolomics data and data description
	4. metab_imput.R -- Imputation of metabolites data 
	5. metab_clustering.R -- Clustering of metabolites 
	6. metab_denoise.R -- Denoising metabolites dataset 
	7. biobank_covars_preprocess.R -- Removing participants without air pollutants from uk biobank
	8. air_met_explore.R -- Matching UK biobank and Nightingale datasets
	9. recoding_health_ouctcome.R -- Extracting HES data and recoding into binary variables 
	10. obj1_uni_results.R -- Matching metabolomics participants to ukbiobank and data description
	
