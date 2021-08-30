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
	
	
	2. master_recoding_outcome.R -- Defining inclusion/exclusion criteria and outcome
	3. recoding_comorbid.R -- Extracting HES data and recoding into binary variables
	4. master_recoding_rfvar.R -- Recoding covariate variables
	5. extract_biomarker.R -- Extracting biomarkers of interest from raw data
	5. master_biomarker.R -- Processing bioamarkers for descriptive analysis
	5. master_imputing_biomarker.R -- Imputing biomarkers for regression analysis
	6. make_multivar_data.R -- Generating dataset for multivariate analysis
	
UNIVARIATE ANALYSIS:
	7. table_1.R -- Table 1
	8. TtD_AaD_histograms.R
	9. master_univ.R + master_univ_viz.R + master_univ_tab.R + make_legend.R -- Univariate logistic regression
	
MULTIVARIATE ANALYSES:
	10. master_denoise.R -- One-hot encoding and Denoising multivariate data
	11. stab_select_lasso.R -- Stability selection logistic LASSO
	12. stab_select_lasso_force.R -- Stability selection logistic LASSO sensitivity analysis
	13. split_train_test.R + stab_select_spls.R -- Stabiliy selection sPLS
	14. lasso_spls_viz.R + lass_spls_pred_perf.R -- LASSO and sPLS visualisations

SENSITIVITY ANALYSES:
  ./TtD_AaD/ Stratification analysis by time to diagnosis and age at diagnosis
	./strat_sex/ Stratification analysis by sex
	./subtype/ Targeted analysis of cancer site
	./nvsm/ Targeted analysis of never-smokers
	./strat_townsend/ Stratification analysis by townsend deprivation index

Source functions:
* penalisation_functions.R -- Functions for stability selection logistic LASSO
* pls_functions.R -- Functions for stability selection PLS
* pls_functions_cali.R -- Functions for stability heatmap sPLS from calibration 
