# Jennifer Selgrath
# Project Seahorse, UBC
# Multiple Stressors and Coral Reefs (Ch5)
# --------------------------------------------
# goal: graphing predictive ability of models with and without landscape variables
# ----------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(lme4)
library(lattice)
library(boot)
library(sjPlot)
library(car)
library(colorspace)


# ------------------------------------------------------------------


# -------------------------------------------
remove(list=ls())
# setwd("C:/Users/jennifer.selgrath/Documents/research/R_projects/phd/stressors_and_coral_reefs")
setwd("C:/Users/jselg/Dropbox/research_x1/R_projects/stressors_and_coral_reefs/")

# ------------------------------------------------------


# load data 
d2<-read_csv("./results_test/m_final_test_data.csv")%>%
  glimpse()

unique(d2$ecological_zone)

P.u<-mean(d2$p_m_final)
P.sd<-sd(d2$p_m_final)
P.sem<-P.u/sqrt(length(d2$p_m_final))

P.stats<-cbind(P.u,P.sd,P.sem)
P.stats

# names(d2)


source("./bin_processing/deets_2025.R")

###########################################
# Graph with estimates

# full model
gg_full<-ggplot(data=d2, aes(x=p_m_final))+geom_histogram(binwidth = .02)+  ylab("Count")+
	xlab("Predicted Probability of Habitats")+
  ylim(0,400)+
  theme_deets(strip_blank = TRUE)  
gg_full


gg_final_no_l<-ggplot(data=d2, aes(x=p_m_final_no_l ))+geom_histogram(binwidth = .02)+  ylab("Count")+
	xlab("Predicted Probability of Habitats")+
  ylim(0,400)+
  theme_deets(strip_blank = TRUE) 
gg_final_no_l



#########################
# reorganize data
d3<-d2%>%
	tidyr::gather(model,estimate,p_m_final:p_m_final_no_l)%>% # ,factor_key=TRUE
  glimpse()


d3$MPA<-as.factor(d3$MPA)
d3$Reef_state<-as.factor(d3$Reef_state)
d3$Ecological_zone<-as.factor(d3$ecological_zone)
glimpse(d3)
head(d3)



d4<-d3%>%
  dplyr::select(ecological_zone,Reef_state,resilience_id,model,estimate)%>%
  mutate(Ecological_zone=factor(ecological_zone),model=factor(model),estimate=as.numeric(estimate))%>%
  mutate(reef_state2=if_else(Reef_state==0,"Rubble", "Coral"))%>% # name for reef state
  glimpse()

range(d4$estimate)

# 
# 
# 
# 
# 
# d4$Reef_state<-"Rubble"
# d4$Reef_state[d4$resilience_id==1]<-"Coral"
# d4$Reef_state<-as.factor(d4$Reef_state)
# 
# glimpse(d4)



#######################
# graphs
source("./bin_processing/deets_2025.R")

d4$fct<-""

# Relevel the factor
d4$Ecological_zone <- factor(
  d4$Ecological_zone,
  levels = c("Outer Reef", 
             "Outer Reef - Channel", 
             "Inner Reef", 
             "Inner Reef - Coastal", 
             "Terrestrial Island", 
             "Coastal"),
  labels = c("Outer Reef",
             "Outer Reef (Channel)",
             "Inner Reef",
             "Inner Reef (Coastal)",
             "Terrestrial Island",
             "Coastal")
)



# Now plot
ggplot(data = d4, aes(x = Ecological_zone, y = estimate, fill = reef_state2)) +
  geom_boxplot(colour = "black", size = .7, outlier.size = 1, na.rm = FALSE) +
  geom_text(data = d4, aes(x = 3.2, y = 1, label = fct), size = 2) +
  ylab("Predicted Probability of Habitats\n") +
  xlab("Ecological Zone") +
  coord_cartesian(ylim = c(-0.01, 1.01)) +
  facet_grid(model ~ ., labeller = labeller(model = "")) +
  scale_fill_discrete_diverging(
    palette = "Berlin", nmax = 5, order = c(5, 4),
    name = "Mapped Habitat"
  ) +
  theme_deets(strip_blank = TRUE, rotate_x = 90) 

ggsave("./doc/fig_3c_3d.png", width = 6, height=8)








