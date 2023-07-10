## ------------------------------------- ##
##  Master file for Edgerton (2023) JCR  ## 
## ------------------------------------- ##

## Clear out working directory
rm(list = ls())
## Description: 
## This file calls in n programs that format and analyze the data 


## Load in R packages
packages <- c('plyr', 'dplyr', 'readxl', 'readr', 'lme4', 'foreign', 
              'fixest', 'pscl', 'ggplot2', 'cowplot', 'states', 'MatchIt',
              'rnaturalearth', 'tmap', 'leaflet', 'mapview', 'shiny',
              'mapdeck', 'leaflet.extras', 'scales', 'texreg')

if (length(setdiff(packages, rownames(installed.packages()))) != 0)
{
  install.packages(setdiff(packages, rownames(installed.packages())))    
}

## Load the R packages
lapply(packages, library, character.only = T)



working_directory <- "Dissertation/Out group foreign fighters/revision/replication/"

setwd(working_directory)

## For replication 
#how_done <- sessionInfo()
#sessionInfo()
#R version 4.2.1 (2022-06-23 ucrt)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22000)
#
#Matrix products: default
#
#locale:
#[1] LC_COLLATE=English_United States.utf8 
#[2] LC_CTYPE=English_United States.utf8   
#[3] LC_MONETARY=English_United States.utf8
#[4] LC_NUMERIC=C                          
#[5] LC_TIME=English_United States.utf8    
#
#attached base packages:
#[1] stats     graphics  grDevices utils     datasets  methods  
#[7] base     
#
#other attached packages:
#[1] rnaturalearth_0.1.0 MatchIt_4.4.0       states_0.3.1       
#[4] cowplot_1.1.1       ggplot2_3.3.6       pscl_1.5.5         
#[7] fixest_0.10.4       foreign_0.8-82      lme4_1.1-30        
#[10] Matrix_1.4-1        readr_2.1.2         readxl_1.4.0       
#[13] dplyr_1.1.0         plyr_1.8.7         
#
#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.9          lattice_0.20-45     tidyr_1.2.0        
#[4] class_7.3-20        zoo_1.8-10          digest_0.6.29      
#[7] utf8_1.2.2          R6_2.5.1            cellranger_1.1.0   
#[10] backports_1.4.1     e1071_1.7-11        httr_1.4.3         
#[13] pillar_1.8.1        rlang_1.0.6         rstudioapi_0.14    
#[16] minqa_1.2.4         nloptr_2.0.3        texreg_1.38.6      
#[19] splines_4.2.1       stringr_1.4.0       munsell_0.5.0      
#[22] proxy_0.4-27        compiler_4.2.1      numDeriv_2016.8-1.1
#[25] pkgconfig_2.0.3     htmltools_0.5.3     tidyselect_1.2.0   
#[28] tibble_3.1.7        fansi_1.0.3         tzdb_0.3.0         
#[31] withr_2.5.0         sf_1.0-8            MASS_7.3-58        
#[34] grid_4.2.1          nlme_3.1-158        gtable_0.3.0       
#[37] lifecycle_1.0.3     DBI_1.1.3           magrittr_2.0.3     
#[40] units_0.8-0         scales_1.2.0        KernSmooth_2.23-20 
#[43] cli_3.6.0           stringi_1.7.8       dreamerr_1.2.3     
#[46] reshape2_1.4.4      sp_1.5-0            ellipsis_0.3.2     
#[49] generics_0.1.3      vctrs_0.5.2         boot_1.3-28        
#[52] sandwich_3.0-2      Formula_1.2-4       RColorBrewer_1.1-3 
#[55] tools_4.2.1         forcats_0.5.1       glue_1.6.2         
#[58] purrr_0.3.4         hms_1.1.1           crosstalk_1.2.0    
#[61] fastmap_1.1.0       colorspace_2.0-3    classInt_0.4-7     
#[64] haven_2.5.0   

## Clean, merge, and format data for analysis
source("1_clean_merge_data.r")
gc()

## Check export
if (sum(list.files() == "data_for_analysis.Rda") != 1) stop("Data for analysis not created.")

## Main and sensitivity analyses
source("2_analyze_data.r")
gc()

## Create the data viz used in the paper
source("3_plot_data.r")
gc()

## Create the tables
source("4_create_tables.r")
gc()
