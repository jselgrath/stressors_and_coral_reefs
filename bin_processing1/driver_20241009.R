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
# center and standardize using z-scale transformation 
# ------------------------------------
source("./bin_processing1/14_All_dataSetupA_train.R") # was 3
source("./bin_processing1/14_All_dataSetupA_test.R")
# input:       ./results_train/13_IndpVar_Pts_all.csv
# output:      ./results_train/14_IndpVar_Pts_train.csv
# output:      ./doc/14_IndpVar_Pts_MeanSD_train.csv        # mean and SD 


# ------------------------------------
# remove olango, combine coastal and terr islands
# remove outlier
# run all data setup
# ------------------------------------
source("./bin_processing1/15_All_dataSetupB_train.R") # was 4
source("./bin_processing1/15_All_dataSetupB_test.R")
# input: ./results_test/14_IndpVar_Pts_test.csv
# output: ./results_test/14_IndpVar_Pts_test.csv


# look at variables
source("./bin_processing1/16_All_correlationsViz.R") 
# input:      ./results_train/14_IndpVar_Pts_train.csv
# output:     ./doc/correlations_train.csv


####################
# Analyses
####################
# not using this, but separates reef flat and reef slope (not needed because model works well together)
# A_analysis_with_geomorphic_separate_option.R

# dredge full model
source("./bin.../A_analysis_2024c.R")

# ------------------------------------
# model with 1 random effect
source("./bin_analysis/Q41_analysis_20180623_mixedEf1.R")
# input: 
# output:            
 
# ------------------------------------
# graph models
source("Q41_ModelVizGraph_miEffects2.R")
# input: 
# output:    

# ------------------------------------
# graph models for paper
source("Q41_Fig4.R")
# input: ./results/RS_only/Q41_model_20161109_mixedEf1_52.R
# output: Fig4

# ------------------------------------
# model residuals
source("Q41_Residuals_ExportingMixedEf.R")
# input: 
# output:    

# source("Q41_Residuals_Graphing.R")
source("Q41_FigS2.R")
# input: 
# output:    

# ------------------------------------
# re-run models with testing data
# ------------------------------------

# First, create data by subbing testing points in for real points by renaming folders
# Note: Easier to do manually by changing folder names so code stays updated.
source("Q_All_dataSetup1_ReduceVariables_20160831.R") 
# input: 
# output: IndpVar_Ptsa_RS.csv

# log transform variables
source("Q_All_dataSetup2_LogTransformations_20161028.R")
source("Q_All_dataSetup3_20161028.R")
source("Q_All_dataSetup4_20161028.R") #was analysis setup
source("Q_All_dataSetup5_20161028_LongNames.R")


################
# Analysis
source("Q51_analysis_m_me_testingData_20161028.R")
#output: Q51_data_test.csv

source("Q51_Fig6_ ModelVizGraph_TestingData.R")









##################################################
#############
# Below are old models
##########
# model with all variables
source("Q11_analysis_20160819.R")
# output: Q11_model_20160816.R = final model (m8)

# examine residuals
source("Q11_residuals_20160826.R")

#setup data for graphing final model
source("Q11_ModelVizDataSetup_20160826.R")

#graphing final model
source("Q11_ModelVizGraph_20160826.R")

#Exporting Residuals to map in ArcGIS
source("Q11_ExportingResiduals_20160816.R")


##############
# Redoing analysis with mixed effects & with no dest fishing
##############
source("Q11_analysis_20160831_MixedEf2a.R")
# output: Q11_model_20160831_mixedEf2a.R

source("Q11_ModelVizDataSetup_MiEffects2_20160830.R") #not needed if use sjp.glmer

source("Q11_ModelVizGraph_MiEffects2a.R") #with sjp.glmer
source("Q11_analysis_20160831_MixedEf2b.R")
#Q11_model_20160831_mixedEf2b.R
source("Q11_ModelVizGraph_MiEffects2b.R")
# several




################################
# Run model with independent data
# This code needs updating for fishing
##########################
# reset data setup to be for Ground Truthing points
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch5/Resilience/bin/")

source("pointJoin21Resilience_GT.R")
source("pointJoin22IndpVarFishingNorm_GT.R")
source("pointJoin23FishingAllYrsNorm_GT.R")
source("pointJoin24FishingYrs_NormCumulative_GT.R")
source('pointJoin25FishingDestYrsNorm_GT.R')
source("pointJoin26FishingYrsDest_NormCumulative_GT.R")
source("pointJoin27IndpVarFragstat_GT.R")
source("pointJoin28IndpVarOther_GT.R")
# source("pointJoin29Risks_GT.R")
source("pointJoin30IndpVarDist_GT.R")
source("pointJoin31_PopRsk_GT.R")

# source("pointJoin32habitatSGall_data_prox_u_20160628.R")
# source("pointJoin33habitatSGall_data_prox_spatial_20160621.R") 
# source("pointJoin34habitatMGall_data_prox_u_20160627.R")
# source("pointJoin35habitatMG_data_prox_spatial_20160627.R") 

# This is just from EcuDist in GIS. Simpler than above. 
# With LEK sg in inner channel and updated mg on olango
source("pointJoin32habitatDist_SgMg_20160831.R")

source("pointJoin36_thermalStress98_07_20160907.R") # output: pts_ThermStress.csv

source("point_MergeAll_GT.R") #output:  IndpVar_Pts_GT.shp/csv  

############
# analyses
############
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch5/Resilience/bin/")

# reduce # of variables to reign thigns in a bit
source("Q31_All_dataSetup_ReduceVariables_GT.R") #updated aug 1 2016
#output: IndpVar_Pts1000b_GT.csv

# remove Olango
source("Q31_analysisSetup_GT.R")

# model with all variables
source("Q31_analysis_m_me2b_GT.R")
# source("Q31_analysis_All.R") #probabilities for non-heirarchical models that fit well. Here with simplified output

#orig
source("Q31_ModelVizGraph_MiEffects2b_GT.R")
source("Q11_ModelVizGraph_MiEffects2b_simple11.R") #or simple12
source("Q11_Residuals_ExportingMixedEf_simple11.R")

# updated with interactions
source("Q31_analysis_20160923_MixedEf1b_simple2b.R")
source("Q31_Residuals_ExportingMixedEf_simple18b2.R")
source("Q31_ModelVizGraph_MiEffects1b_simple18b.R")

# graph predicted values
# source("Q31_ModelVizGraph_GT.R")
# output (in doc): PredictedProb_box.png
