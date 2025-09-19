# Jennifer Selgrath 
# Calculate cumulative impact fishing .tif files

# GOAL: To create normalized, cumulative, and lag rasters of 1960-2010 values for fishing effort (days per year)
# gears used here: all gears, blast fishing, fishing with poison

###############################################

library(terra)
library(sf)
# ---------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")
#


# # ---------------------------------------
# all fishing 
files.a<-list.files("./gis2/fishing/effort_fa","est_dayYr_all_.*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.a; s.a<-rast(files.a)
varnames(s.a) <- c('all_1960','all_1970', 'all_1980','all_1990','all_2000',"all_2010")
names(s.a) <- c('all_1960','all_1970', 'all_1980','all_1990','all_2000',"all_2010")
plot(s.a)

# # blast fishing
files.b<-list.files("./gis2/fishing/effort_fa","est_dayYr_blast.*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.b; s.b<-rast(files.b)
varnames(s.b) <- c('blast_1960','blast_1970', 'blast_1980','blast_1990','blast_2000',"blast_2010")
names(s.b) <- c('blast_1960','blast_1970', 'blast_1980','blast_1990','blast_2000',"blast_2010")
plot(s.b)

# # poison fishing
files.p<-list.files("./gis2/fishing/effort_fa","est_dayYr_poison.*\\.tif$", full.names = TRUE, ignore.case = TRUE); files.p; s.p<-rast(files.p)
varnames(s.p)= c('poison_1960','poison_1970', 'poison_1980','poison_1990','poison_2000',"poison_2010")
names(s.p)= c('poison_1960','poison_1970', 'poison_1980','poison_1990','poison_2000',"poison_2010")
plot(s.p)

#read in file of coral/rubble area only
CA<-st_read("./results/habitat.gpkg",layer="co_ru_fa_reclass2")%>%
  glimpse()



# -------------------------------
# Calculating Max Effort
# -------------------------------

# # Mask rasters so only evaluating max fishing effort in coral areas
s.a2<-mask(s.a, CA) 
str(s.a2)

s.b2<-mask(s.b, CA) 
str(s.b2)

s.p2<-mask(s.p, CA) 
str(s.p2)

# Calculate max for each year for Coral and Rubble Area only
mx_a<-tibble()
mx_b<-tibble()
mx_p<-tibble()

# all ---------------------------
for (i in 1:6){
  val_a<-minmax(s.a2[[i]])
  val2_a<-cbind(val_a[1],val_a[2])
  file_a=names(s.a2[[i]])
  max_a<-cbind(val2_a,file_a)
  mx_a<-rbind(mx_a,max_a)
}
mx_a
names(mx_a)<-c("min_a","max_a","file_a")

mx_a$max_a<-as.numeric(mx_a$max_a)
mx_a$min_a<-as.numeric(mx_a$min_a)
glimpse(mx_a)


# blast ---------------------------
for (i in 1:6){
  val_b<-minmax(s.b2[[i]])
  val2_b<-cbind(val_b[1],val_b[2])
  file_b=names(s.b2[[i]])
  max_b<-cbind(val2_b,file_b)
  mx_b<-rbind(mx_b,max_b)
}
mx_b
names(mx_b)<-c("min_b","max_b","file_b")

mx_b$max_b<-as.numeric(mx_b$max_b)
mx_b$min_b<-as.numeric(mx_b$min_b)
glimpse(mx_b)


# poison ---------------------------
for (i in 1:6){
  val_p<-minmax(s.p2[[i]])
  val2_p<-cbind(val_p[1],val_p[2])
  file_p=names(s.p2[[i]])
  max_p<-cbind(val2_p,file_p)
  mx_p<-rbind(mx_p,max_p)
}
mx_p
names(mx_p)<-c("min_p","max_p","file_p")

# max 
mx_p$max_p<-as.numeric(mx_p$max_p)
mx_p$min_p<-as.numeric(mx_p$min_p)
glimpse(mx_p)



# max for all years
mx.a<-max(mx_a$max_a)
mx.b<-max(mx_b$max_b)
mx.p<-max(mx_p$max_p)

# save max values
write_csv(mx_a,"./doc/fishing_all_max_all_yr_coralarea.csv")
write_csv(mx_b,"./doc/fishing_blast_max_all_yr_coralarea.csv")
write_csv(mx_p,"./doc/fishing_poison_max_all_yr_coralarea.csv")

# ------------------------------------
# normalize  whole map by max in coral/rubble area --
# -----------------------------------
s.a3<-s.a/mx.a
s.b3<-s.b/mx.b
s.p3<-s.p/mx.p

# varnames(s.a3) <- c('all_1960_nrmA','all_1970_nrmA', 'all_1980_nrmA','all_1990_nrmA','all_2000_nrmA',"all_2010_nrmA")
names(s.a3) <-    c('all_1960_nrmA','all_1970_nrmA', 'all_1980_nrmA','all_1990_nrmA','all_2000_nrmA',"all_2010_nrmA")
nm.a <-    c('all_1960_nrmA','all_1970_nrmA', 'all_1980_nrmA','all_1990_nrmA','all_2000_nrmA',"all_2010_nrmA")

# varnames(s.b3) <- c('blast_1960_nrmA','blast_1970_nrmA', 'blast_1980_nrmA','blast_1990_nrmA','blast_2000_nrmA','blast_2010_nrmA')
names(s.b3) <-    c('blast_1960_nrmA','blast_1970_nrmA', 'blast_1980_nrmA','blast_1990_nrmA','blast_2000_nrmA','blast_2010_nrmA')
nm.b <-    c('blast_1960_nrmA','blast_1970_nrmA', 'blast_1980_nrmA','blast_1990_nrmA','blast_2000_nrmA','blast_2010_nrmA')

# varnames(s.p3)= c('poison_1960_nrmA','poison_1970_nrmA', 'poison_1980_nrmA','poison_1990_nrmA','poison_2000_nrmA','poison_2010_nrmA')
names(s.p3)<-    c('poison_1960_nrmA','poison_1970_nrmA', 'poison_1980_nrmA','poison_1990_nrmA','poison_2000_nrmA','poison_2010_nrmA')
nm.p<-    c('poison_1960_nrmA','poison_1970_nrmA', 'poison_1980_nrmA','poison_1990_nrmA','poison_2000_nrmA','poison_2010_nrmA')

#export normalized rasters -----------------------------------

# all
for (i in 1:nlyr(s.a3)) {
  layer_name <- nm.a[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_normalized/", nm.a[i], ".tif") 
  writeRaster(s.a3[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# blast
for (i in 1:nlyr(s.b3)) {
  layer_name <- names(s.b3)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_normalized/", nm.b[i], ".tif") 
  writeRaster(s.b3[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# poison
for (i in 1:nlyr(s.p3)) {
  layer_name <- names(s.p3)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_normalized/", nm.p[i], ".tif") 
  writeRaster(s.p3[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# ------------------------------------
# cumulative normalized fishing  ---------------------
# ------------------------------------

cumulativef<-function(x,group1){

	fYr00A=x[[6]]
	fYr10A=x[[6]]+x[[5]]
	fYr20A=x[[6]]+x[[5]]+x[[4]]
	fYr30A=x[[6]]+x[[5]]+x[[4]]+x[[3]]
	fYr40A=x[[6]]+x[[5]]+x[[4]]+x[[3]]+x[[2]] 
	fYr50A=x[[6]]+x[[5]]+x[[4]]+x[[3]]+x[[2]]+x[[1]]

	# stack
	x2<-c(fYr00A,fYr10A,fYr20A,fYr30A,fYr40A,fYr50A)
	grp=group1 #this names the gear subset calc
	
	# update names
	names(x2)<-c(paste("cumulative_",grp,"_00",sep=""),paste("cumulative_",grp,"_10",sep=""),paste("cumulative_",grp,"_20",sep=""),paste("cumulative_",grp,"_30",sep=""),paste("cumulative_",grp,"_40",sep=""),paste("cumulative_",grp,"_50",sep=""))
	varnames(x2)<-c(paste("cumulative_",grp,"_00",sep=""),paste("cumulative_",grp,"_10",sep=""),paste("cumulative_",grp,"_20",sep=""),paste("cumulative_",grp,"_30",sep=""),paste("cumulative_",grp,"_40",sep=""),paste("cumulative_",grp,"_50",sep=""))
	
	return(x2)
}

# calc cumulative values - for normalized data
cumulative.a<-cumulativef(x=s.a3,group1="all"); cumulative.a
cumulative.b<-cumulativef(x=s.b3,group1="blast"); cumulative.b
cumulative.p<-cumulativef(x=s.p3,group1="poison"); cumulative.p
# cumulative.k<-cumulativef(x=s.k,group1="kaykay"); cumulative.k



# -- export cumulative rasters -----------------------

# all
for (i in 1:nlyr(cumulative.a)) {
  layer_name <- names(cumulative.a)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_cumulative/", layer_name, ".tif") 
  writeRaster(cumulative.a[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# blast
for (i in 1:nlyr(cumulative.b)) {
  layer_name <- names(cumulative.b)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_cumulative/", layer_name, ".tif") 
  writeRaster(cumulative.b[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# poison
for (i in 1:nlyr(cumulative.p)) {
  layer_name <- names(cumulative.p)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_cumulative/", layer_name, ".tif") 
  writeRaster(cumulative.p[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}



# -------------------------------
# Calculating Lag Effort
# -------------------------------

lagf<-function(x,group1){
  
  fYrLag10A=x[[5]]
  fYrLag20A=x[[5]]+x[[4]]
  fYrLag30A=x[[5]]+x[[4]]+x[[3]]
  fYrLag40A=x[[5]]+x[[4]]+x[[3]]+x[[2]]
  fYrLag50A=x[[5]]+x[[4]]+x[[3]]+x[[2]]+x[[1]] 

   # stack
  x2<-c(fYrLag10A,fYrLag20A,fYrLag30A,fYrLag40A,fYrLag50A)
  grp=group1 #this names the gear subset calc
  
  # update names
  names(x2)<-c(paste("lag_",grp,"_10",sep=""),paste("lag_",grp,"_20",sep=""),paste("lag_",grp,"_30",sep=""),paste("lag_",grp,"_40",sep=""),paste("lag_",grp,"_50",sep=""))
  varnames(x2)<-c(paste("lag_",grp,"_10",sep=""),paste("lag_",grp,"_20",sep=""),paste("lag_",grp,"_30",sep=""),paste("lag_",grp,"_40",sep=""),paste("lag_",grp,"_50",sep=""))
  
  return(x2)
}

# calc lag values - for normalized data
lag.a<-lagf(x=s.a3,group1="all"); lag.a
lag.b<-lagf(x=s.b3,group1="blast"); lag.b
lag.p<-lagf(x=s.p3,group1="poison"); lag.p
# lag.k<-lagf(x=s.k,group1="kaykay"); lag.k

# -- export lag rasters - for normalized data -----------------------
# all
for (i in 1:nlyr(lag.a)) {
  layer_name <- names(lag.a)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_lag/", layer_name, ".tif") 
  writeRaster(lag.a[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# blast
for (i in 1:nlyr(lag.b)) {
  layer_name <- names(lag.b)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_lag/", layer_name, ".tif") 
  writeRaster(lag.b[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

# poison
for (i in 1:nlyr(lag.p)) {
  layer_name <- names(lag.p)[i]
  output_filename <- paste0("./gis2/fishing/effort_fa_lag/", layer_name, ".tif") 
  writeRaster(lag.p[[i]], output_filename, overwrite=TRUE) # Set overwrite=TRUE if needed
}

