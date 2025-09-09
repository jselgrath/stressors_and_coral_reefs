# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------

# goal - organize so numeric and categorical variables are grouped


# ------------------------------------------------------
remove(list=ls())

# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")

setwd("C:/Users/jselg/OneDrive/Documents/research/R_projects/phd/stressors_and_coral_reefs")

# ------------------------------------------------------

# load data 
d1<-read_csv("./results_test/14_IndpVar_Pts_test.csv")%>%
  # combine terrestrial island and coastal ecological zones
  mutate(EcoZone2=if_else(EcoZone2=="Terrestrial Island","Coastal",EcoZone2))%>% 


  # remove outlier - removing other ones with modeling
  filter(PtID2!=1384)%>%
  

  
  # reassigning geomorphology based on depth
  mutate(Geomorphic2=if_else(-2.5>Depth_m,"Reef Slope",Geomorphic))%>%
  mutate(Geomorphic2=if_else(-.5<Depth_m,"Reef Flat",Geomorphic2))%>%
  
  # removing coastal zone -------------------
# this improves model fit, but reduces R2 and also means model does not include one habiat area. So not excluding.
# filter(EcoZone2!="Coastal")%>%

# change name of response variable and ecological zone
  mutate(Reef_state=as.factor(Id_resil),
         ecological_zone=as.factor(EcoZone2),
         MPA=as.factor(if_else(mpa=="unprotected","open",mpa)),
         municipality=as.factor(Id_MunWtr))%>%

glimpse()

# check new assignment of reef flat etc
cbind(d1$Geomorphic,d1$Geomorphic2,d1$Depth_m)

# graph updated variable ------------------
ggplot(d1,aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()

d1%>%
  filter(Geomorphic2=="Reef Slope")%>%
  ggplot(aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()

d1%>%
  filter(Geomorphic2=="Reef Flat")%>%
  ggplot(aes(x,y,color=Depth_m,shape=Geomorphic2 ))+geom_point()



range(d1$Depth_m,na.rm=T)


# ---------------------------------------------
# subset data 
# ---------------------------------------------
d2<-d1%>%
  dplyr::select(PtID2,Reef_state,ecological_zone,longitude, mpa_barangay,MPA, mpa_area_ha, municipality,Depth_m,sg_minDist_100,mg_minDist_100,co_minDist_100,  PopRskDecay, PopRskDecay.Nrm,fYrLag30A, cum_blast00, cum_FA_blast00,cum_FA_blast10,cum_kaykay00, cum_poison00,divGen2010,CoRuArea,CoRuEdg2Area,FRAC,PROX,SHAPE, PARA,CoRuEdg2Area,Geomorphic,Geomorphic2,x,y)%>%
  glimpse()


# ---------------------------------------------
# futher subset data -  select non-correlated variables (see Correlation Viz)
# ---------------------------------------------
d3<- d2%>%
  dplyr::select(PtID2,Reef_state,ecological_zone,MPA,mpa_area_ha,Depth_m,sg_minDist_100,mg_minDist_100,PopRskDecay.Nrm,fYrLag30A, cum_FA_blast10,divGen2010,Geomorphic2,SHAPE,x,y)%>%
  glimpse()

d3<- d2%>%
  dplyr::select(PtID2,Reef_state,ecological_zone,MPA,Depth_m,sg_minDist_100,PopRskDecay.Nrm,fYrLag30A, cum_FA_blast10,Geomorphic2,SHAPE,x,y)%>%
  glimpse()


# remove nas
d4<-na.omit(d3)



########################
# Save
write_csv(d4,"./results_test/16_IndpVar_Pts_test.csv")