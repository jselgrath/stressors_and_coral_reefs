# ------------------------------------
# join points with fishing effort data from 2010, gear diversity. 
# FYI - not using in analysis because better measures below.
# ------------------------------------
# source("./bin_processing1/2_pointJoin_IndpVarFishingNorm_train.R")
# source("./bin_processing1/2_pointJoin_IndpVarFishingNorm_test.R")
# input:      ./results/rs_only/hab_Resil_Pts_RS.gpkg
#             ./gis/fishing_cum/CoralRubArea.shp
#             ./gis/fishing2010/ > effort rasters for gears, diversity, etc
# output:     ./results_train/2_pts_FishingImpact_normalized.csv




# ------------------------------------
# make points to join fragstat data to

# source("./bin_processing1/7_1point_create_train_test.R")
# in ArcPRO use these files to calculate NEAR distance to mangroves (mg) and seagrass (sg)
# do not run here because it will overwrite that calculation

# two version of test data - 25m (~1000 points from 2024) & 50m (~500 pts from PhD)


##################################
# Task 1.3 load shapefile with landscape variables

# in GIS run model "Ch4_DistToHabitat/CoRuFrag_ToPts.tbx"
# this joins testing points (50 away) with CoRu_Frag_20160927.shp> CoRu_Frag_Pts.shp


# frag<-st_read("./gis/landscape/CoRu_Frag_20160927.shp")%>%
#   select(SHAPE=SHAPE_1,TYPE:geometry)%>%
#   glimpse()
# plot(frag) #


# ------------------------------------


# ------------------------------------
# # calculate prox of SG. RS data only - old version. see code above to do in R (current version)
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


