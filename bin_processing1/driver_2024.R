# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# --------------------------------------------
# driver

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------

# SET FOLDER TO TRAIN OR TEST DATA BASED ON WHAT WANT TO SOURCE

# ------------------------------------
# see folder: FishingMeasures for code to extract various measures of fishing effort
# fishing measures code is NOT updated for new R packages
# ------------------------------------

# ------------------------------------
# Before run this code, First set random points in ArcGIS and save in ".../Ch5/Resilience/data/randompoints/")
# ------------------------------------


# ------------------------------------
# join points to coral/rubble data
# ------------------------------------
source("./bin_processing1/1_pointJoin_resilience_train.R")
source("./bin_processing1/1_pointJoin_resilience_test.R")
# input:  ./gis/random_points/RanPts_RSonly_19x22_NoOlango.shp
#         ./gis/resilience/RS_FocalArea_Updated_20160627b.shp
#         ./gis/resilience/CoRu_Smplfd_20160628.shp
# output: ./results_train/hab_Resil_Pts_RS.gpkg



# ------------------------------------
# join points with fishing effort data from 2010, gear diversity. 
# FYI - not using in analysis because better measures below.
# ------------------------------------
source("./bin_processing1/2_pointJoin_IndpVarFishingNorm_train.R")
source("./bin_processing1/2_pointJoin_IndpVarFishingNorm_test.R")
# input:      ./results/rs_only/hab_Resil_Pts_RS.gpkg
#             ./gis/fishing_cum/CoralRubArea.shp
#             ./gis/fishing2010/ > effort rasters for gears, diversity, etc
# output:     ./results_train/2_pts_FishingImpact_normalized.csv



# ------------------------------------
# join points with fishing effort data (all fishing) from decades
# create normalized impact values for each year '.Nrm' and for all years '.NrmA'
# ------------------------------------
source("./bin_processing1/3_pointJoin_FishingAllYrs_1Norm_train.R")
source("./bin_processing1/3_pointJoin_FishingAllYrs_1Norm_test.R")
# input:      ./results/hab_Resil_Pts_RS.gpkg
#             ./gis/coralrubble/CoralRubArea.shp
#             ./fishing/EffortEstimates/YEAR
# output:     ./results_train/3_pts_FishingYrs_1normalized.csv

# ------------------------------------
# cumulative fishing ?and lag? (additive) - ALL gears
# ------------------------------------
source("./bin_processing1/3_pointJoin_FishingAllYrs_2Cumulative_train.R")
source("./bin_processing1/3_pointJoin_FishingAllYrs_2Cumulative_test.R")
# input:      ./results/hab_Resil_Pts_RS.gpkg
#             ./gis/coralrubble/CoralRubArea.shp   
#             
# output:     ./results/3pts_FishingYrs_2cumulative.csv


# ------------------------------------
# join points with fishing effort data (g1n and some g5n and categories from Ch3) from decades
# ------------------------------------

# ------------------------------------
# create normalized impact values for each year '.Nrm' and for all years '.NrmA'
# ------------------------------------
source("./bin_processing1/4_pointJoin_Fishing_g1n_1cumulative_train.R")
source("./bin_processing1/4_pointJoin_Fishing_g1n_1cumulative_test.R")
# input:      ./results/hab_Resil_Pts_RS.gpkg
#             ./gis/coralrubble/CoralRubArea.shp   
#             
# output:     ./results/4_pts_cumFishing_g1n_GEAR.csv

# source("./bin_processing1/pointJoin4Fishing_g1n_2Lag_20240929.R")
# input:      ./results/hab_Resil_Pts_RS.gpkg
#                
# output: pts_lagFishing_g1n_g5poison.csv


# ------------------------------------
# join points with fishing effort data (destructive) from decades
# ------------------------------------
source('./bin_processing1/5_pointJoin_FishingDestYrsNorm_train.R')
source('./bin_processing1/5_pointJoin_FishingDestYrsNorm_test.R')
# input:      ./results/rs_only/hab_Resil_Pts_RS.gpkg
#             ./gis/coralrubble/CoralRubArea.shp   
#             ./gis/fishing/effort_estimates/ ... destYEAR.tif    
# output:     ./results_train/5_pts_FishingYrsDest_normalized.csv

# ------------------------------------
# cumulative fishing (additive) - DESTRUCTIVE GEARS
# ------------------------------------
source("./bin_processing1/6_pointJoin_FishingYrsDest_NormCumulative_train.R")
source("./bin_processing1/6_pointJoin_FishingYrsDest_NormCumulative_test.R")
# input:      ./results/rs_only/hab_Resil_Pts_RS.gpkg
#             ./gis/coralrubble/CoralRubArea.shp   
#             ./results_train/5_pts_FishingYrsDest_normalized.csv   
# output:     ./results_train/6_pts_FishingYrs_destructive_cumulative.csv

# ------------------------------------
# make points to join fragstat data to

# source("./bin_processing1/7_1point_create_train_test.R")
# in ArcPRO use these files to calculate NEAR distance to mangroves (mg) and seagrass (sg)
# do not run here because it will overwrite that calculation

# two version of test data - 25m (~1000 points from 2024) & 50m (~500 pts from PhD)
# ------------------------------------


# ------------------------------------
# join points with fragstats output 
# ------------------------------------
source("./bin_processing1/7_pointJoin_IndpVarFragstat_train.R") 
source("./bin_processing1/7_pointJoin_IndpVarFragstat_test.R")
# input:      ./results_XXX/hab_Resil_Pts_RS.gpkg
#             ./gis/coralrubble/CoralRubArea.shp   
#             ./gis/co_rub_frag_points/CoRu_Frag_Pts.shp
# output:     ./results_XXX/7_pts_fragstats.csv # ...2 for test = 50m


# ------------------------------------
# join to other indp var
# ------------------------------------
source("./bin_processing1/8_pointJoin_IndpVarOther_train.R")
source("./bin_processing1/8_pointJoin_IndpVarOther_test.R")
# input:      ./results/rs_only/hab_Resil_Pts_RS.gpkg
#             depth_splinelandshallow_20160726_dis
#             EcologicalZones_FA_Land_20160810
#             longzoneFA_20160525
#             MPA_FA_20160525_3
#             MunWaterLine3_Polyg6
# output:     ./results_train/IndpVarOther_Pts_RS.gpkg

# ------------------------------------
# source Population Density Decay based risk map. This is better that reefs at risk
# ------------------------------------
# PopRsk2: based on buffers. 
# PopRskDecay: based on 1/sqrt(distance)*population density rank
# 20180621 - changed pop risk to entire study area (no longer clipped)
source("./bin_processing1/11_pointJoin_PopRsk_train.R")
source("./bin_processing1/11_pointJoin_PopRsk_test.R")
# input:  ./results/rs_only/hab_Resil_Pts_RS.gpkg                
#         ./gis/coralrubble/CoralRubArea.shp    
#         ,.gis/population/PopRskDecay_NoClip.tif
# output: ./results/RS_only/pts_PopRsk_Norm.csv

# ------------------------------------
# # calculate prox of SG. RS data only
# ------------------------------------
# # input .csv from GenerateNearTable Tool in ArcGIS.
# source("pointJoin12habitatSGall_data_prox_u_20160628.R")
# source("pointJoin13habitatSGall_data_prox_spatial_20160621.R") 
# 
# #and for MG
# source("pointJoin14habitatMGall_data_prox_u_20160627.R")
# source("pointJoin15habitatMG_data_prox_spatial_20160627.R") 

# ------------------------------------
# # calculate proximity (prox) of SG. RS data only
# This is just from EcoDist in GIS. Simpler than above. 
# With LEK sg in inner channel and updated mg on olango
# ------------------------------------
source("./bin_processing1/12_1pointJoin_habitatDist_SgMgCo_train.R")
source("./bin_processing1/12_1pointJoin_habitatDist_SgMgCo_test.R")
# input:  ./results/rs_only/hab_Resil_Pts_RS.gpkg                
#         ./gis/coralrubble/CoralRubArea.shp           
#         ./gis/prox/Co_dist_RS.tif
#         ./gis/prox/MG_dist_RS_wOlangoUpdates.tif
#         ./gis/prox/SG_dist_RS_wLEKinner.tif
# output: ./results/RS_only/pts_Co_Mg_Sg_minDist.csv

# ------------------------------------
#update dist to coral for coral locations with data from Fragstats
# ------------------------------------
source("./bin_processing1/12_2pointJoin_habitatDist_updateCoWithENN_train.R")
source("./bin_processing1/12_2pointJoin_habitatDist_updateCoWithENN_test.R")
# input:   
# output: 

# -----------------------------------
# add in zones for seagrass and mangrove distance
# -----------------------------------
source("./bin_processing1/13_pointJoin_sg_mg_buffer_distance_train_test.R")
# input:   ./gis/0most_shp/seagrass_buffer/seagrass_buf_FA2_20160525.shp
#          ./gis/0most_shp/mangrove_buffer/mangrove_buf_FA2_20160525.shp
#         ./results_test/hab_Resil_Pts_RS.gpkg
#         ./results_train/hab_Resil_Pts_RS.gpkg
# output: ./results_train/13_distance_sg_mg_buf.gpkg # or test
#         ./results_train/13_distance_sg_mg_buf.csv


# ------------------------------------
# merge all points together
# ------------------------------------
source("./bin_processing1/13_point_MergeAll_train.R") 
source("./bin_processing1/13_point_MergeAll_test.R") 
# input:    MANY
# output: ./results/IndpVar_Pts_RS.gpkg
#         ./results/IndpVar_Pts_RS.csv


# ------------------------------------
# ANALYSIS SET UP
# ------------------------------------

# ------------------------------------
# log transform - may add this back, but currently not using
# -------------------------------
# source("./bin_processing1/14_0_All_dataSetup_LogTransformations_Train.R")

# ------------------------------------
# organizing variables by categorical and numeric
# ------------------------------------
source("./bin_processing1/14_All_dataSetupA_train.R") 
source("./bin_processing1/14_All_dataSetupA_test.R")
# input:       ./results_train/13_IndpVar_Pts_all.csv
# output:      ./results_train/14_IndpVar_Pts_train.csv




# ------------------------------------
# look at variables
# ------------------------------------
source("./bin_processing1/15_All_correlationsViz_train.R") 
# input:      ./results_train/14_IndpVar_Pts_train.csv
# output:     ./doc/correlations_train_7_threshold.csv


# ------------------------------------
# remove olango, combine coastal and terr islands
# remove outlier
# run all data setup
# ------------------------------------
source("./bin_processing1/16_All_dataSetupB_train.R") # was 4
source("./bin_processing1/16_All_dataSetupB_test.R")
# input: ./results_test/14_IndpVar_Pts_test.csv
# output: ./results_test/16_IndpVar_Pts_test.csv





# ------------------------------------
# calc summary statistics - NEED TO  FINISH THIS CODE
# ------------------------------------
# output:      ./doc/14_IndpVar_Pts_MeanSD_train.csv        # mean and SD 


####################
# Analyses
####################
# A. dredge full model ------------------------
source("./bin_analysis/A_analysis_2024f.R")
# input:  ./results_train/16_IndpVar_Pts_train.csv
# output: ./results_train/mixedEf_final_all.R
#         ./results_train/mixedEf_final_no_landscape.R
#         ./results_train/mixedEf_final_all1.RData
#         ./results_train/17_IndpVar_Pts_train_for_models_all.csv
#         ./results_train/17_IndpVar_Pts_train_for_models_subset.csv

# B. Figure 3a - full model------------------------
source("./bin_analysis/B_fig_3a_full_model.R")
# input:  ./results_train/mixedEf_final_all.R
# output: ./doc/fig_3a_full_model.tif


# B. Figure 3b - reduced model------------------------
source("./bin_analysis/B_fig_3b_reduced_model.R")
# input:  ./results_train/mixedEf_final_no_landscape.R
# output: ./doc/fig_3b_reduced_model.tif

# C. export residuals
source("./bin_analysis/C_residuals_exporting_final_model.R")
# input:  ./results_train/mixedEf_final_all.R
#         ./results_train/mixedEf_final_no_landscape.R - not used
#         ./results_train/15_IndpVar_Pts_train_for_models_subset.csv
# output: ./results_train/full_model_residuals.csv
#         ./results_train/full_model_residuals.shp
#         ./results_train/full_model_residuals.gpkg

# D. calculate wald scores
source("./bin_analysis/D_TableS3_WaldScores_final_model.R")
# input:  ./results_train/mixedEf_final_all.R
# output: ./doc/TableS3_final_model_wald.csv


# E. replace testing data in model
source("./bin_analysis/E_analysis_testing_data.R")
# input:  ./results_train/mixedEf_final_all.R") # full model
#         ./results_train/mixedEf_final_no_landscape.R
#         ./results_test/14_IndpVar_Pts_test.csv
# output: ./results_test/m_final_test_data.csv

# graphs of predictive power
source("./bin_analysis/F_Fig3c_d_prediction.R")
# input:    ./results_test/m_final_test_data.csv
# output:   ./results_test/Fig3c3d_.tiff

# uncenter and undstandardize coefficents ------------------------
source("./bin_analysis/G_unscale_uncenter_parameters.R")
# input:  ./results_train/15_IndpVar_Pts_train_for_models_all.csv
#         ./results_train/mixedEf_final_all1.R
# output: 






