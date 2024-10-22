# Multiple stressors on coral reefs
# goal: log transform variables for models
# 
#####################################
remove(list=ls())

loc=("C:/wd/Resilience/")
setwd(loc)


# load data 
d1<-read.csv("./results/RS_only/IndpVar_Ptsa_RS.csv",header = T) 
head(d1)
names(d1)
d1<-na.omit(d1)

         
######################################################

################
# transforming
###############
d1$Depth_m<- -1*d1$Depth_m # make depth positive
d1$Ldepth<-log((d1$Depth_m)+1)

d1$LCoRuArea<-log(d1$CoRuArea+1)
d1$LCoRuLngth<-log(d1$CoRuLngth+1)
d1$LdistBgy.km<-log(d1$distBgy.km+1)
d1$LdistRiver.km<-log(d1$distRiver.km+1)
d1$LdistTown.km<-log(d1$distTown.km+1)
d1$LdistMarket.km<--1*abs(log(d1$distMarket.km+1)) #make dist to market negative
d1$LPopRsk.Nrm<-log(d1$PopRsk.Nrm+1)


d1$Lsg_minDist100<-log((d1$sg_minDist/100)+1)
d1$Lmg_minDist100<-log((d1$mg_minDist/100)+1)
d1$Lco_minDist100<-log((d1$co_minDist/100)+1)

# log landscape variables
d1$LAREA<-log(d1$AREA+1)
d1$LPARA<-log(d1$PARA+1)
d1$LENN<-log(d1$ENN+1)
d1$LSHAPE<-log(d1$SHAPE+1)

# edge/area
d1$CoRuEdg2Area<-d1$CoRuLngth/d1$CoRuArea
d1$LCoRuEdg2Area<-log((d1$CoRuLngth/d1$CoRuArea)+1)

################
# g1n fishing variables
##################
# cumulative
d1$Lcum_blast00<-log(d1$cum_blast00+1)
d1$Lcum_blast10<-log(d1$cum_blast10+1)
d1$Lcum_blast20<-log(d1$cum_blast20+1)
d1$Lcum_blast30<-log(d1$cum_blast30+1)

d1$Lcum_kaykay00<-log(d1$cum_kaykay00+1)
d1$Lcum_kaykay10<-log(d1$cum_kaykay10+1)
d1$Lcum_kaykay20<-log(d1$cum_kaykay20+1)
d1$Lcum_kaykay30<-log(d1$cum_kaykay30+1)

d1$Lcum_poison00<-log(d1$cum_poison00+1)
d1$Lcum_poison10<-log(d1$cum_poison10+1)
d1$Lcum_poison20<-log(d1$cum_poison20+1)
d1$Lcum_poison30<-log(d1$cum_poison30+1)

# legacy
d1$Llag_blast10<-log(d1$lag_blast10+1)
d1$Llag_blast20<-log(d1$lag_blast20+1)
d1$Llag_blast30<-log(d1$lag_blast30+1)

d1$Llag_kaykay10<-log(d1$lag_kaykay10+1)
d1$Llag_kaykay20<-log(d1$lag_kaykay20+1)
d1$Llag_kaykay30<-log(d1$lag_kaykay30+1)

d1$Llag_poison10<-log(d1$lag_poison10+1)
d1$Llag_poison20<-log(d1$lag_poison20+1)
d1$Llag_poison30<-log(d1$lag_poison30+1)

# dif  #see below for notes on transformation
d1$tempb<- 1; d1$tempb[d1$dif_blast10_00<0] <- -1
d1$tempk<- 1; d1$tempk[d1$dif_kaykay10_00<0]<- -1
d1$tempp<- 1; d1$tempp[d1$dif_poison10_00<0]<- -1

d1$Ldif_blast10_00<-log(d1$dif_blast10_00+1)*d1$tempb
d1$Ldif_kaykay10_00<-log(d1$dif_kaykay10_00+1)*d1$tempk
d1$Ldif_poison10_00<-log(d1$dif_poison10_00+1)*d1$tempp

qplot(d1$Ldif_kaykay10_00)

#############################
# Calculated fishing variables
##############################
d1$LfYr00A<-log(d1$fYr00A+1)
d1$LfYr10A<-log(d1$fYr10A+1)
d1$LfYr20A<-log(d1$fYr20A+1)
d1$LfYr30A<-log(d1$fYr30A+1)
d1$LfYr40A<-log(d1$fYr40A+1)
d1$LfYr50A<-log(d1$fYr50A+1)

# dest fishing
d1$LdfYr00A<-log(d1$dfYr00A+1)
d1$LdfYr10A<-log(d1$dfYr10A+1)
d1$LdfYr20A<-log(d1$dfYr20A+1)
d1$LdfYr30A<-log(d1$dfYr30A+1)
d1$LdfYr40A<-log(d1$dfYr40A+1)
d1$LdfYr50A<-log(d1$dfYr50A+1)

# Lag fishing variables
qplot(log(d1$fYrLag30A+1))
qplot(log(d1$fYrLag30A+1))

d1$LfYrLag10A<-log(d1$fYrLag10A+1)
d1$LfYrLag20A<-log(d1$fYrLag20A+1)
d1$LfYrLag30A<-log(d1$fYrLag30A+1)
d1$LfYrLag40A<-log(d1$fYrLag40A+1)
d1$LfYrLag50A<-log(d1$fYrLag50A+1)

d1$LdfYrLag10A<-log(d1$dfYrLag10A+1)
d1$LdfYrLag20A<-log(d1$dfYrLag20A+1)
d1$LdfYrLag30A<-log(d1$dfYrLag30A+1)
d1$LdfYrLag40A<-log(d1$dfYrLag40A+1)
d1$LdfYrLag50A<-log(d1$dfYrLag50A+1)

# changes
# due to changes with negative values use log-modulus transformation
# http://www.statsblogs.com/2014/07/14/a-log-transformation-of-positive-and-negative-values/

# d1$temp1<- 1
# d1$temp1[d1$allDif_10_90<0]<- -1
# d1$LallDif_00_90<- log(d1$allDif_00_90+1)*d1$temp1 
# 
# d1$temp2<- 1
# d1$temp2[d1$allDif_10_00<0]<- -1
# d1$LallDif_10_00<- log(d1$allDif_10_00+1)*d1$temp2 
# 
# d1$temp3<- 1
# d1$temp3[d1$allDif_10_80<0]<- -1
# d1$LallDif_10_80<- log(abs(d1$allDif_10_80 +1))*d1$temp3
# 
# d1$temp4<- 1
# d1$temp4[d1$allDif_10_90<0]<- -1
# d1$LallDif_10_90<- log(abs(d1$allDif_10_90+1))*d1$temp4
# 
# d1$temp5<- 1
# d1$temp5[d1$destDif_00_90<0]<- -1
# d1$LdestDif_00_90<-log(d1$destDif_00_90+1)*d1$temp5
# 
# d1$temp6<- 1
# d1$temp6[d1$destDif_10_00<0]<- -1
# d1$LdestDif_10_00<-log(d1$destDif_10_00 +1)*d1$temp6 
# 
# d1$temp7<- 1
# d1$temp7[d1$destDif_10_80<0]<- -1
# d1$LdestDif_10_80<-log(d1$destDif_10_80+1)*d1$temp7
# 
# d1$temp8<- 1
# d1$temp8[d1$destDif_10_90<0]<- -1
# d1$LdestDif_10_90<-log(d1$destDif_10_90+1)*d1$temp8
# # d1$LdestDif_90_80<-log(d1$destDif_90_80+1)
# 
# 
# # drop temp columns
# 
# names(d1)
# 
# d2<-dplyr::select(d1, -tempb,-tempk,-tempp,-temp8,-temp7,-temp6,-temp5,-temp4,-temp3,-temp2,-temp1)
# names(d2)

# check if depth is positive
d2$Ldepth

# remove non-logged value
d2<-dplyr::select(d2,-(CoRuEdg2Area))

# Save
setwd(loc)
write.table(d2,file="./results/RS_only/IndpVar_Ptsb_RS.csv",col.names = T,row.names = F, sep=",")

#############################################
#reset wd
setwd("C:/wd/Resilience/bin/")
