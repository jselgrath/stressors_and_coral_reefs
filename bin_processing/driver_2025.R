# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# --------------------------------------------
# driver

# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------
# see folder: FishingMeasures for code to extract various measures of fishing effort
# fishing measures code is NOT updated for new R packages
# ------------------------------------


# -----------------------------------------------
# -- preliminary data processing -----------------------
# ------------------------------------------------
# -- calculate focal area for analysis at size of fishing maps (smaller than other FA file)
source("./bin_processing/000_focal_area_for_analysis.R")
# input:./gis2/fishing/effort/...[1].tif"
# output: ./gis2/focal_area/focal_area_sm.shp
#         ./results/basic_files.gpkg,layer="focal_area_sm"


# organize habitat file for entire db area that was mapped for habitat - bigger than focal area for analysis, but this allows correct measures of landscape variables like distance 
source("./bin_processing/000_organize_habitat_map.R")
# input: "./gis2/habitat/db_full_area/all_full_area/habitat_full_area_rs_lek_reclass_20250615_union_with_fa2.shp"
# output: ./results/habitat.gpkg",layer="habitat_all_db_reclass2
#         ./results/habitat.gpkg",layer="habitat_all_db_reclass
#         ./results/habitat.gpkg",layer="habitat_all_fa_reclass2
#         ./gis2/habitat/db_full_area/all_full_area/habitat.shp # same as reclass 2

# select coral and rubble area only
source("./bin_processing/000_select_co_ru_habitat.R")
# input:  "./results/habitat.gpkg",layer="habitat_all_fa_reclass2"
# output: "./results/habitat.gpkg",layer="co_ru_fa_reclass2"


# -- landscape variables --------------------------------
# Calculate patch area and perimeter & nearest-patch edge-to-edge distance (same habitat)
source("./bin_processing/00_IndpVarLandscape.R")
# input: ./results/habitat.gpkg","habitat_all_db_reclass2
# output: ./results/habitat.gpkg",layer="habitat_all_db_landscape1"


# calculate landscape variables: distance from coral and rubble patches to seagrass and mangrove patches (edge to edge)
source("./bin_processing/00_IndpVarLandscape2.R")
# input:  ./results/habitat.gpkg",layer="habitat_all_db_landscape1"
# output: ./results/habitat.gpkg",layer="habitat_all_db_landscape2"

# population risk -----------------
# calculate population risk using three metrics - original pop density, pop density with more accurate inhabited area (removing uninhabited islands) "inhab", and population
source("./bin_processing/00_population_risk")
# input:  ./gis2/focal_area/focal_area_lg_land2.shp   # land is binary variable of land and water - bigger than focal area to include outside villages
#         ./gis2/focal_area/focal_area_sm_land2.shp
#         ./gis2/population/BarangayPopulation2010_pt_.shp
#         ./gis2/population/BarangayPopulation2010p.shp
#         ./gis2/population/BarangayPopulation2010_1p.shp   # removes mangrove only islands from area calculations
#        
# output: ./gis2/population_risk/pop_risk_dens_orig_fa.tif
#         ./gis2/population_risk/pop_risk_dens_inhab_fa.tif
#         ./gis2/population_risk/pop_risk_pop_fa.tif
#         ./gis2/population/barangay_pop2010_area.shp
#         ./results/basic_files.gpkg",layer="barangay_pop2010_area"
#         ./doc/barangay_pop2010_area.csv
    

# clip population density risk rasters to reef area, calculate min and max values ----------------------------------
source("./bin_processing/00_population_risk2.R")
# input:  ./gis2/population_risk/pop_risk_dens_orig_fa.tif
#         ./gis2/population_risk/pop_risk_dens_inhab_fa.tif
#         ./gis2/population_risk/pop_risk_pop_fa.tif
#         ./results/habitat.gpkg",layer="co_ru_fa_reclass2   #file of coral/rubble area only for clipping
# output: ./gis2/population_risk/pop_risk_dens_inhab_reef.tif
#         ./gis2/population_risk/pop_risk_dens_orig_reef.tif
#         ./gis2/population_risk/pop_risk_pop_reef.tif

#         ./doc/population_risk_orig_range.csv  # min and max values
#         ./doc/population_risk_inhab_range.csv
#         ./doc/population_risk_pop_range.csv

# calculate normalized, cumulative, and lag fishing effort maps based on max values for the coral and rubble area only. 
# cumulative and lag values use the normalized maps as input
source("./bin_processing/00_calc_fishing_normalized_cumulative_lag.R")
# input:   ./gis2/fishing/effort_fa","est_dayYr_all_.*\\.tif$
#          ./gis2/fishing/effort_fa","est_dayYr_blast.*\\.tif$
#          ./gis2/fishing/effort_fa","est_dayYr_poison.*\\.tif$
#          ./results/habitat.gpkg",layer="co_ru_fa_reclass2"
# 
# output:  ./doc/fishing_all_max_all_yr_coralarea.csv - and blast and poison
#          ./gis2/fishing/effort_fa_normalized/", layer_name, ".tif"
#          ./gis2/fishing/effort_fa_cumulative/", layer_name, ".tif"
#          ./gis2/fishing/effort_fa_lag/", layer_name, ".tif"
#         
#         
#         


# -- random points for analysis ----------------------------------------------------------

# Create random points list in R inside coral and rubble habitat in focal area, stratified to include MPAs to ensure enough samples in MPAs
source("./bin_processing/0_random_points.R")
# input:  ./gis2/focal_area/focal_area_sm.shp
#         ./results/habitat.gpkg",layer="habitat_all_fa_reclass2"
#         ./gis2/mpa/MPA_FA_20160525_3.shp"
# output: paste0(./results/stratified_random_points_",n_total,"pts_",min_dist,"m_train.shp)  # and test
#         ./results/basic_files.gpkg",layer=paste0("stratified_random_points_",n_total,"pts_",min_dist,"m_train" # and test



# ------------------------------------------------------------------------
# --  join points to data and point specific calculations --------------------
# ------------------------------------------------------------------------


# ------------------------------------
# join points to coral/rubble data - now restricting analysis to focal area
# ------------------------------------
source("./bin_processing/1_pointJoin_resilience_train_test.R")
# input:  ./results/train.gpkg, layer="stratified_random_points_1500pts_100m_train"  
#         ./results/train.gpkg, layer="stratified_random_points_1500pts_100m_test" 
#         ./gis/resilience/RS_FocalArea_Updated_20160627b.shp
#         ./gis/resilience/CoRu_Smplfd_20160628.shp
# output: ./results/train.gpkg, layer="1_pts_habitat_tr"
#         ./results/test.gpkg,  layer="1_pts_habitat_te"


# ------------------------------------
# join points with fishing effort data from decades
# for normalized, cumulative, and lag fishing data
 #  - normalized by all years '.NrmA'
# ------------------------------------
source("./bin_processing/2_pointJoin_fishing_train_test.R")
# output: ./results_train/2_pts_fishingeffort_normalized.csv")
#         ./results_test/2_pts_fishingeffort_normalized.csv")
#         ./results_train/3_pts_fishingeffort_cumulative.csv")
#         ./results_test/3_pts_fishingeffort_cumulative.csv")
#         ./results_train/4_pts_fishingeffort_lag.csv")
#         ./results_test/4_pts_fishingeffort_lag.csv")


# 
# 
# source("./bin_processing/2_pointJoin_FishingAllYrs_1Norm_train_test.R")
# # input:  ./results/train.gpkg, layer="stratified_random_points_1500pts_100m_train"  
# #         ./results/train.gpkg, layer="stratified_random_points_1500pts_100m_test
# #             ./gis/coralrubble/CoralRubArea.shp
# #             ./fishing/EffortEstimates/YEAR
# # output:     ./results_train/2_pts_FishingYrs_1normalized.csv
# #             ./results_test/2_pts_FishingYrs_1normalized.csv
# 
# # ------------------------------------------
# # rename cumulative files to 'cumulative'  > only need to run one time
# #source(./bin_processing/rename_cumulative_files.R)
# 
# 
# # ------------------------------------
# # cumulative fishing and lag fishing (additive) - ALL gears
# # ------------------------------------
# source("./bin_processing/3_pointJoin_FishingAllYrs_2Cumulative_train_test.R")
# # input:      ./results_train/2_pts_FishingYrs_1normalized.csv
# #             ./results_test/2_pts_FishingYrs_1normalized.csv
# # output:     ./results_train/3pts_FishingYrs_cumulative.csv
# #             ./results_test/3pts_FishingYrs_cumulative.csv
# 
# # ------------------------------------
# # join points with fishing effort data (g1n and some g5n and categories from Ch3) from decades
# # ------------------------------------
# 
# # ------------------------------------
# # create normalized impact values for each year '.Nrm' and for all years '.NrmA'
# # ------------------------------------
# source("./bin_processing/4_pointJoin_Fishing_g1n_1cumulative_train_test.R")
# # input:      ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_train"
# #             ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_test"
# #             ./gis/coralrubble/CoralRubArea.shp   
# # output:     ./results_train/4_pts_cumFishing_g1n_GEAR.csv
# #             ./results_test/4_pts_cumFishing_g1n_GEAR.csv
# 
# # ------------------------------------
# # join points with fishing effort data (destructive) from decades
# # ------------------------------------
# source('./bin_processing/5_pointJoin_FishingDestYrsNorm_train_test.R')
# # input:      ./results/rs_only/hab_Resil_Pts_RS.gpkg
# #             ./gis/coralrubble/CoralRubArea.shp   
# #             ./gis/fishing/effort_estimates/ ... destYEAR.tif    
# # output:     ./results_train/5_pts_FishingYrsDest_normalized.csv
# #             ./results_test/5_pts_FishingYrsDest_normalized.csv
# 
# # ------------------------------------
# # cumulative fishing (additive) - DESTRUCTIVE GEARS
# # ------------------------------------
# source("./bin_processing/6_pointJoin_FishingYrsDest_NormCumulative_train_test.R")
# # input:      ./results_train/5_pts_FishingYrsDest_normalized.csv
# #             ./results_test/5_pts_FishingYrsDest_normalized.csv   
# # output:     ./results_train/6_pts_FishingYrs_destructive_cumulative.csv
# #             ./results_test/6_pts_FishingYrs_destructive_cumulative.csv
# 
# ------------------------------------
# join points with landscape metrics  
# ------------------------------------
source("./bin_processing/7_pointJoin_IndpVarLandscape_train_test.R") 
# input:      ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_train"
#             ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_test" 
#             ./results/habitat.gpkg",layer="habitat_all_db_landscape2"
# output:     ./results_train/7_pts_fragstats.csv
#             ./results_test/7_pts_fragstats.csv


# ------------------------------------
# join points with distance to nearest habitat edge 
# ------------------------------------
# calculate distance between points and habitate edges
source("./bin_processing/8_pointJoin_point_habitatDist_SgMgCo_train_test.R")
# input:      ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_train"
#             ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_test" 
#             ./results/habitat.gpkg",layer="habitat_all_db_reclass2" # file for larger area than focal area for distance
# output:     ./results/train.gpkg", layer="pts2_landscape_edge_dist"
#             ./results/test.gpkg",  layer="pts2_landscape_edge_dist"
#             ./results_train/8_pts_landscape_edge_dist.csv
#             ./results_test/8_pts_landscape_edge_dist.csv

# ------------------------------------
# join to other indp var
# ------------------------------------
source("./bin_processing/9_pointJoin_IndpVarOther_train_test.R")
# input:      ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_train"
#             ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_test" 
#             ./gis2/depth/depth_shp/depth_splinelandshallow_20160726_dis.shp
#             ./gis2/ecological_zones/EcoZones2_DB.shp     #EcologicalZones_FA_Land_20160810
#             ./gis2/longitude_zones/longzoneFA_20160525.shp
#             ./gis2/mpa/MPA_FA_20160525_3.shp
#             ./gis2/municipal_waters/municipal_waters.shp
# output:     ./results/train.gpkg", layer="10_pts_IndpVarOther"
#             ./results/test.gpkg", layer="10_pts_IndpVarOther"
#             ./results_train/9_pts_IndpVarOther_pts.csv
#             ./results_test/9_pts_IndpVarOther_pts.csv


# ------------------------------------
# source Population Density Decay 
# ------------------------------------
# old files:
# PopRsk2: based on buffers. 
# PopRskDecay: based on 1/sqrt(distance)*population density rank
# 20180621 - changed pop risk to entire study area (no longer clipped)

# current file: 
# calculated 3 versions in R - see code above in 00 section (updated to be reproducible)
source("./bin_processing/10_pointJoin_PopRsk_train_test.R")
# input:  ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_train"
#         ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_test"               
#         ./gis2/population_risk/pop_risk_dens_inhab_reef.tif
#         ./gis2/population_risk/pop_risk_dens_orig_reef.tif
#         ./gis2/population_risk/pop_risk_pop_reef.tif

#         ./doc/population_risk_orig_range.csv  # min and max values
#         ./doc/population_risk_inhab_range.csv
#         ./doc/population_risk_pop_range.csv

# output: ./results_train/10_pts_PopRsk_Norm.csv
#         ./results_test/10_pts_PopRsk_Norm.csv
#         

# ------------------------------------
# join distance to nearest river to points -------
# ------------------------------------
source("./bin_processing/11_pointJoin_river_distance_train_test.R")
# input:  ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_train"
#         ./results/basic_files.gpkg", layer="stratified_random_points_900pts_250m_test"               
#         
# output: "./results_train/11_pts_river_distance_1normalized.csv")
#         ./results_test/11_pts_river_distance_1normalized.csv")


# ------------------------------------
# merge all points together
# ------------------------------------
source("./bin_processing/13_point_MergeAll_train_test.R") 
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
source("./bin_processing/14_All_dataSetupA_train_test.R") 
# input:       ./results_train/13_IndpVar_Pts_all.csv
# output:      ./results_train/14_IndpVar_Pts_train.csv




# ------------------------------------
# look at variables
# ------------------------------------
source("./bin_processing/15_All_correlationsViz_train_test.R") 
# input:      ./results_train/14_IndpVar_Pts_train.csv
# output:     ./doc/correlations_train_7_threshold.csv


# ------------------------------------
# remove olango, combine coastal and terr islands
# remove outlier
# remove NAs
# remove correlated variables 
# run all data setup
# calculate percent cover stats
# ------------------------------------
source("./bin_processing/16_All_dataSetupB_train_test.R") # was 4
# input: ./results_test/14_IndpVar_Pts_test.csv
# output: ./results_test/16_IndpVar_Pts_test.csv
#         ./doc/percentage_stats.csv





# ------------------------------------
# calc summary statistics - NEED TO  FINISH THIS CODE
# ------------------------------------
# output:      ./doc/14_IndpVar_Pts_MeanSD_train.csv        # mean and SD 



# ANALYSES -------------------------

# A. dredge full model ------------------------
source("./bin_analysis/A_analysis.R")
# input:  ./results_train/16_IndpVar_Pts_train.csv
# output: ./results_train/model_full.R                              # full model
#         ./results_train/model_full_avg.rds                        # averaged full model
#         ./results_train/model_no_landscape.R                      # no landscape model
#         ./results_train/model_no_landscape_avg.rds                # averaged no landscape model
#         ./results_train/17_IndpVar_Pts_train_for_models_all.csv   # modified dataset with no outliers, centered and scaled variables
#         ./results_train/mixedEf_final_all1.RData")                # image of workspace
#         ./results_train/mixedEf_final_all1_no_landscape.RData     # later image of workspace

# B. Figure 3a & b - full model and reduced ------------------------
source("./bin_analysis/B_Fig3_2025.R")
# input:  ./results_train/model_full_avg.rds
#         ./results_train/model_no_landscape_avg.rds
# output: ./doc/fig_3a_full_model.tif


# B. Table 1. Full Model ------------------------
source("./bin_analysis/B_Table1_2025.R")
# input:  ./results_train/mixedEf_final_no_landscape.R
# output: ./doc/fig_3b_reduced_model.tif


# C. export residuals
source("./bin_analysis/C_residuals_2025.R")
# input:  ./results_train/model_full.R #full model
#         ./results_train/model_no_landscape.R
#         ../results_train/17_IndpVar_Pts_train_for_models_all.csv"
# output: 
#         ./results_train/full_model_residuals.csv #full model, no landscape variables
#         ./results_train/full_model_residuals.shp
#         ./results_train/full_model_residuals.gpkg, layer="model"
#         ./results_train/full_model_residuals_data2.shp
#         ./results_train/full_model_residuals2.gpkg", layer="all"

# # D. calculate wald scores
# source("./bin_analysis/D_TableS3_WaldScores_final_model.R")
# # input:  ./results_train/mixedEf_final_all.R
# # output: ./doc/TableS3_final_model_wald.csv


# E. replace testing data in model
source("./bin_analysis/E_analysis_testing_data.R")
# input:  ./results_train/model_full.R #full model
#         ./results_train/model_no_landscape.R
#         ./results_test/16_IndpVar_Pts_test_all.csv
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






