# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

###########################################
# # GOAL: reaname cumulative files
###########################################

# -------------------------------------------
# Load packages

library(stars)
library(ggplot2)
library(terra)
library(tidyverse)
library(sf)
# -------------------------------------------


#######################################################
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")


# input and output folders
in_dir  <- "./gis2/fishing/cumulative"
out_dir <- "./gis2/fishing/cumulative_g1"

# make sure output folder exists
if (!dir.exists(out_dir)) dir.create(out_dir)

# list input rasters
files = list.files(in_dir,pattern='\\.tif$', full.names = TRUE, ignore.case = TRUE)%>%
  glimpse()
files





if (length(files) == 0) {
  stop("No files found matching '^cum...\\.tif$' in folder: ", in_dir)
}

# keep a log of mappings
log_map <- data.frame(old = character(0), new = character(0), stringsAsFactors = FALSE)

for (f in files) {
  fname <- basename(f)
  
  # 1) If it starts with 'cum' (any punctuation/case), replace that leading 'cum'
  if (grepl("(?i)^cum", fname, perl = TRUE)) {
    new_fname <- sub("(?i)^cum", "cumulative", fname, perl = TRUE)
  } else if (grepl("(?i)cum", fname, perl = TRUE)) {
    # 2) otherwise, replace the first occurrence of 'cum' anywhere (case-insensitive)
    new_fname <- sub("(?i)cum", "cumulative", fname, perl = TRUE)
  } else {
    # 3) last resort: prefix the filename with 'cumulative_'
    new_fname <- paste0("cumulative", fname)
  }
  
  out_path <- file.path(out_dir, new_fname)
  
  message(sprintf("Writing: '%s'  ->  '%s'", fname, new_fname))
  
  r <- rast(f)
  
  # write with default options; add gdal options if you want compression, etc.
  writeRaster(r, out_path, overwrite = TRUE)
  
  log_map <- rbind(log_map, data.frame(old = fname, new = new_fname, stringsAsFactors = FALSE))
}

# show summary
print(log_map, row.names = FALSE)
cat("\nFiles now in", out_dir, ":\n")
print(list.files(out_dir, pattern = "\\.tif$", full.names = FALSE))