# Multiple stressors on coral reefs
# Jennifer Selgrath
# Project Seahorse, UBC

# goal: driver script to show order of other scripts and to document input and output data
####################################################################################
# SET FOLDER TO TRAIN OR TEST DATA BASED ON WHAT WANT TO SOURCE

# libraries
source("loadlibrariesatstart.R")

###########
# code not include here:
# 1. code to extract various measures of fishing effort
# 2. generated random points in ArcGIS
# 3. code to join points to spatial datasets
###########


############
# analyses setup
############
remove(list=ls())
setwd("C:/wd/Resilience/bin/")

# reduce # of variables to reign things in a bit
source("1_All_dataSetup1_ReduceVariables.R") 
# input:  ./results/RS_only/IndpVar_Pts_RS.csv
# output: ./results/RS_only/IndpVar_Ptsa_RS.csv

# log transform variables
source("2_All_dataSetup2_LogTransformations.R")
#input:  ./results/RS_only/IndpVar_Ptsa_RS.csv
#output: ./results/RS_only/IndpVar_Ptsb_RS.csv

# center and standardize using z-scale transformation 
source("3_All_dataSetup3_Centering.R")
#input:  ./results/RS_only/IndpVar_Ptsb_RS.csv
#output: ./results/RS_only/IndpVar_Ptsc_RS.csv # exporting z-transformed and orig log data only
#output: ./results/RS_only/IndpVar_Ptsc_MeanSD.csv  # mean and SD of log tranformed data for back transforming later

# remove olango, combine coastal and terr islands
# run all data setup
source("4_All_dataSetup4_ReduceCategories.R") 
# input:  ./results/RS_only/IndpVar_Ptsc_RS.csv
# output: ./results/RS_only/IndpVar_Ptsd_RS.csv = Data without any points on Olango
# output: ./results/RS_only/IndpVar_Ptsd_NoCoastalTerr_RS.csv = Data without Olango, Coastal, or Terr. Islands

#remove outlier, make formal names for final models 
source("5_All_dataSetup5_LongNames.R") 
# input:  ./results/RS_only/IndpVar_Ptsd_RS.csv
# output: ./results/RS_only/IndpVar_Ptse_RS.csv = Data with long names
# output: ./results/RS_only/IndpVar_Ptsf_RS.csv = Long names only

####################
# Analyses
####################
setwd("C:/wd/Resilience/bin/")

# model with 1 random effect
source("A_analysis.R")
# input:  ./results/RS_only/IndpVar_Ptse_RS.csv
# output: ./results/RS_only/Q41_model_52.R # final model
# output: ./results/RS_only/Q41_model_50.R # final model, no landscape variables

# graph model
source("./results/RS_only/B_Fig3a_m52.R")
# input: ./results/RS_only/Q41_model_52.R
# output: paste("./doc/Fig3a_BlPop_",dateToday,"_%d.tiff",sep="")

# graph reduced model
source("B_Fig3b_reducedModel_m50.R")
# input: ./results/RS_only/Q41_model_50.R
# output: paste("./doc/Fig3b_BlPop_",dateToday,"_%d.tiff",sep="")

# output residuals from model to graph in ArcMap
source("C_Residuals_Exporting_m52")
# input:  ./results/RS_only/Q41_model_52.R
# input:  ./results/RS_only/Q41_model_50.R
# input:  ./results/RS_only/IndpVar_Ptsf_RS.csv
# output: ./results/RS_only/ModelResiduals_m52.csv


# Next Step: Make figure in ArcGIS that includes 3 maps depicting point locations and values from d3. # Maps: (a) Small residuals (b) Bright spots (c) Dark spots

# Calculate and export wald scores
source("D_TableS3_WaldScores_m52")
# input:  ./results/RS_only/Q41_model_52.R
# output: ./results/RS_only/TableS3_m52.csv

#########
# re-run models with testing data

# First, create data by subbing testing points in for real points by renaming folders and repeating code above to link spatial data to new random points and preparing data for analysis

# Note: Easier to do this manually by changing folder names so code stays updated.

################
# Analysis using new random data
source("E_analysis_testingData.R")
# input:  ./results/RS_only_test/IndpVar_Ptse_RS.csv
# input:  ./results/RS_only/Q41_model_52.R # full model
# input:  ./results/RS_only/Q41_model_50.R # reduced model
# output: ./results/RS_only_test/Q51_data_test.csv

# Graphing predictive ability of full and reduced models
source("F_Fig3c_3d_Prediction.R")
# input: ./results/RS_only_test/Q51_data_test.csv
# output: paste("./doc/Fig3c3d_",dateToday,".tiff",sep="")