### Summmer Project -- Table 1 
### 2nd Aug  - Alicia 


library(gtsummary)
library(htmltools)
library(gt)

# Loading datasets --------------------------------------------------------

path <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/"
path_to_results <- "/rds/general/project/hda_students_data/live/Group2/Alicia/Summer_project/Results/table1/"

metab_clus = readRDS(paste(path, "Results/denoised_clus_no_bio.rds", sep =""))
biobank = readRDS(paste(path, "Results/biobank_final.rds", sep =""))
head(biobank)


# Data processing for Townsend index  --------------------------------------------------------

biobank$townsend = ifelse(biobank$`189-0.0`>-2.2079, 1,
                          ifelse(biobank$`189-0.0`<=-2.2079, 0, NA))


col_names_factor <- c('31-0.0','24014-0.0','6138-0.0','20116-0.0', 'townsend')
biobank[,col_names_factor] <- lapply(biobank[,col_names_factor] , factor)

biobank$townsend <- 
  factor(biobank$townsend, 
         levels=c(0,1),
         labels=c("Low Deprivation scores", # Reference
                  "High Deprivation scores"))
table1_var<- c('21022-0.0', '31-0.0','21001-0.0','6138-0.0','20116-0.0',
               '24003-0.0','24004-0.0','24005-0.0','24006-0.0','24007-0.0','24008-0.0','24014-0.0',
               'townsend')
df = biobank[table1_var]

# Using gt_summary for Townsend index-------------------------------------------------------
reset_gtsummary_theme()

tab1<- df %>%
  tbl_summary(
    by = townsend,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    # type= list(all_continuous() ~ "continuous", all_categorical() ~ "categorical",
    label= list(`21022-0.0` ~ "Age",`31-0.0` ~ "Sex",`21001-0.0` ~ "BMI",
                `6138-0.0`~"Education", `20116-0.0`~"Smoking Status",
                `24003-0.0` ~ "NO2", `24005-0.0` ~ "PM10", `24006-0.0` ~ "PM2.5", `24004-0.0` ~ "NOx",`24007-0.0` ~ "PM2.5 absorbance", `24008-0.0` ~ "PM2.5-10", `24014-0.0` ~"Close to major road"),
    missing = "no")%>%
  modify_header(label ~ "") %>%
  modify_header(update = all_stat_cols() ~ "**{level}**<br>(N = {n})")%>%
  modify_footnote(
    update = all_stat_cols() ~ "Mean (SD) for continuous variables; n (%) for categorical variables"
  ) %>%
  add_p(test = all_continuous() ~ "t.test",
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  # add_n(statistic = "{n} ({p_miss}%)", col_label = "**Total<br>(% missing value)**") %>%
  add_overall()%>% 
  bold_labels() %>%
  bold_p() %>%
  as_gt() %>%
  as_latex()

sink(paste(path_to_results, "tab1_TDI_report_updated.txt", sep =""))
tab1 %>%
  as.character() %>%
  cat()
sink()



# Data processing for male female  --------------------------------------------------------

biobank$sex = ifelse(biobank$`31-0.0` == "Female", 0,
                          ifelse(biobank$`31-0.0` == "Male", 1, NA))


col_names_factor <- c('24014-0.0','6138-0.0','20116-0.0', 'sex')
biobank[,col_names_factor] <- lapply(biobank[,col_names_factor] , factor)

biobank$sex <- 
  factor(biobank$sex, 
         levels=c(0,1),
         labels=c("Female", # Reference
                  "Male"))
table1_var<- c('21022-0.0','21001-0.0','6138-0.0','20116-0.0','189-0.0',
               '24003-0.0','24004-0.0','24005-0.0','24006-0.0','24007-0.0','24008-0.0','24014-0.0',
               'sex')
df = biobank[table1_var]

# Using gt_summary for male female-------------------------------------------------------
reset_gtsummary_theme()

tab1<- df %>%
  tbl_summary(
    by = sex,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    # type= list(all_continuous() ~ "continuous", all_categorical() ~ "categorical",
    label= list(`21022-0.0` ~ "Age",`21001-0.0` ~ "BMI", `189-0.0` ~ "Townsend Deprivation Index",
                `6138-0.0`~"Education", `20116-0.0`~"Smoking Status",
                `24003-0.0` ~ "NO2", `24005-0.0` ~ "PM10", `24006-0.0` ~ "PM2.5", `24004-0.0` ~ "NOx",`24007-0.0` ~ "PM2.5 absorbance", `24008-0.0` ~ "PM2.5-10", `24014-0.0` ~"Close to major road"),
    missing = "no")%>%
  modify_header(label ~ "") %>%
  modify_header(update = all_stat_cols() ~ "**{level}**<br>(N = {n})")%>%
  modify_footnote(
    update = all_stat_cols() ~ "Mean (SD) for continuous variables; n (%) for categorical variables"
  ) %>%
  add_p(test = all_continuous() ~ "t.test",
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  # add_n(statistic = "{n} ({p_miss}%)", col_label = "**Total<br>(% missing value)**") %>%
  bold_labels() %>%
  bold_p() %>%
  as_gt() %>%
  as_latex()

sink(paste(path_to_results, "tab1_sex_report.txt", sep =""))
tab1 %>%
  as.character() %>%
  cat()
sink()


# Data processing for PM2.5  --------------------------------------------------------

biobank$pm2.5 = ifelse(biobank$`24006-0.0`>10, 1,0)

col_names_factor <- c('31-0.0','24014-0.0','6138-0.0','21001-0.0','20116-0.0', 'pm2.5')
biobank[,col_names_factor] <- lapply(biobank[,col_names_factor] , factor)

biobank$pm2.5 <- 
  factor(biobank$pm2.5, 
         levels=c(0,1),
         labels=c("PM2.5 below WHO guideline", # Reference
                  "PM2.5 above WHO guideline"))
table1_var<- c('21022-0.0', '31-0.0','21001-0.0','6138-0.0','189-0.0','20116-0.0',
               '24003-0.0','24004-0.0','24005-0.0','24007-0.0','24008-0.0','24014-0.0',
               'pm2.5')
df = biobank[table1_var]

# Using gt_summary using PM2.5 -------------------------------------------------------
reset_gtsummary_theme()

tab2<- df %>%
  tbl_summary(
    by = pm2.5,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    # type= list(all_continuous() ~ "continuous", all_categorical() ~ "categorical",
    label= list(`21022-0.0` ~ "Age",`31-0.0` ~ "Sex",`21001-0.0` ~ "BMI",
                `6138-0.0`~"Education", `189-0.0`~"Townsend Indes",`20116-0.0`~"Smoking Status",
                `24003-0.0` ~ "NO2", `24005-0.0` ~ "PM10", `24004-0.0` ~ "NOx",`24007-0.0` ~ "PM2.5 absorbance", `24008-0.0` ~ "PM2.5-10", `24014-0.0` ~"Close to major road"),
    missing = "no")%>%
  modify_header(label ~ "") %>%
  modify_header(update = all_stat_cols() ~ "**{level}**<br>(N = {n})")%>%
  modify_footnote(
    update = all_stat_cols() ~ "Mean (SD) for continuous variables; n (%) for categorical variables"
  ) %>%
  add_p(test = all_continuous() ~ "t.test",
        pvalue_fun = function(x) style_pvalue(x, digits = 2)) %>%
  # add_n(statistic = "{n} ({p_miss}%)", col_label = "**Total<br>(% missing value)**") %>%
  bold_labels() %>%
  bold_p() %>%
  as_gt() %>%
  as_latex()

sink(paste(path_to_results, "tab1_pm2.5_report.txt", sep =""))
tab2 %>%
  as.character() %>%
  cat()
sink()
