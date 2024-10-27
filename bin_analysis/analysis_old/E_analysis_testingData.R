# Multiple stressors on coral reefs
# goal:   Rebuilding models with testing data to assess model predictive power

####################################################################

##########################
remove(list=ls())

loc=("C:/wd/Resilience/")
setwd(loc)

# load data 
d2<-read.csv("./results/RS_only_test/IndpVar_Ptse_RS.csv",header = T) 
names(d2)

#make var into factors
d2$State<-as.factor(d2$State)
d2$MPA<-as.factor(d2$MPA)


#source orig model
setwd(loc)

# final forward stepping model
# load final models
load("./results/RS_only/Q41_model_52.R") # full model
load("./results/RS_only/Q41_model_50.R") # no landscape variables


# Re-Build model with new data
d2$Pm52<- round(predict(m.me_52, newdata = d2, type = "response"),3)
qplot(d2$Pm52)
d2$Pm50<- round(predict(m.me_50, newdata = d2, type = "response"),3)
qplot(d2$Pm50)

# from checking model from Sept meeting with SG - much worse than other models!
# d2$Pm11<- round(predict(m.me2b, newdata = d2, type = "response"),3)
names(d2)


######################
# Save predicted results to examine in GIS
setwd(loc1)
write.table(d2,file="./results/RS_only_test/Q51_data_test.csv",col.names = T,row.names = F,sep=",")


#########
setwd("C:/wd/Resilience/bin/")
