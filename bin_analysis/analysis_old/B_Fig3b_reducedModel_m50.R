# Multiple stressors on coral reefs
# goal: make figure 3b - reduced model
# effects are standardized
#############################
library(dplyr);library(ggplot2)

#######
# Load models
################################
remove(list=ls())

loc=("C:/wd/Resilience/")
setwd(loc)

dateToday<- Sys.Date()

# load final models
load("./results/RS_only/Q41_model_50.R") #reduced model

##############
# calculate se and CI

# http://www.talkstats.com/showthread.php/25420-Confidence-Interval-for-model-parameters-(in-R-lme4)

models<-list(m.me_50)
models2<-"m.me_50"
# models3<-c("(c) Fishing legacy","(d) Market proximity"," (a) Fishing legacy, interaction"," (b) Market proximity, interaction")

# run this before loop
est<-data.frame()
# for (i in 1:1){
lower <- coef(summary(models[[1]]))[,1] + qnorm(.025)*coef(summary(models[[1]]))[,2]
upper <- coef(summary(models[[1]]))[,1] + qnorm(.975)*coef(summary(models[[1]]))[,2]
e1<-data.frame(cbind(coef(summary(models[[1]])), lower, upper))
e1$Parameter<-row.names(e1)
e1$model<-models2[1]
# e1$grph<-models3[i]
est<-rbind(est,e1)

rownames(est) <- c()
est

# est$grph<-as.factor(est$grph)
# levels(est$grph)

est$Parameter<-as.factor(est$Parameter)
levels(est$Parameter)

# order the varaibles sensically
est$ordr<-(c(1,5,6,4,7,8,3,2)) #
est
est$Parameter<-reorder(as.factor(est$Parameter), as.numeric(est$ordr), FUN=mean)
levels(est$Parameter)
dplyr::select(est,ordr, Parameter, model)

# create labels for graphs
est$labl<-as.character(est$Parameter)
est$labl<-gsub("Population.density","Population density",est$labl)
est$labl<-gsub("MPA601","MPA",est$labl)
est$labl<-gsub("Market.proximity","Market proximity",est$labl)
est$labl<-gsub("Patch.shape","Patch shape",est$labl)
est$labl<-gsub("Isolation.from.seagrass","Seagrass isolation",est$labl)
est$labl<-gsub("Fishing.legacy.1980.2000","Fishing legacy, 1980-2000",est$labl)
est$labl<-gsub("Fishing.legacy.1980.2000:Population.density","Fishing legacy, 1980-2000:Population density",est$labl) 
est$labl<-gsub("z.Lcum_blast10","Blast fishing, 2000-2010",est$labl)
est$labl<-gsub("Population density:Blast fishing, 2000-2010","Blast fishing, 2000-2010:Population density", est$labl)

# check labels
est$labl

# make column for insignificant values
est$sig<-1
est$sig[est$labl == "Population density"]<-0
# est$sig[est$labl == "Fishing legacy, 1980-2000:Population density"]<-0
est$sig[est$labl == "Fishing legacy, 1980-2000"]<-0
est$sig<-as.factor(est$sig)

# remove intercept for graph
est2<-filter(est,Parameter!="(Intercept)")

est2

###############
# graphing setup
deets2<- theme(
	axis.text =  element_text(size=rel(1.9), colour="black"), 
	axis.title=element_text(size=rel(2),colour="black"), 
	    panel.grid=element_blank(), 
			panel.background=element_rect(fill="#f7f7f7"),
			panel.border=element_rect(fill=NA, colour = "black"),
			# axis.line =  element_line(size=2),
			axis.ticks = element_line(size=1),
	legend.position="none"
			)


##########
# Graphing
ppi=300
tiff(paste("./doc/Fig3b_blastxpop_",dateToday,"_%d.tiff",sep=""), width = (3)*ppi, height=(1.55)*ppi)

# est2 doesn't have intercept
g1<- 	ggplot(data = est2, aes(x=Parameter, y = Estimate,ymin=lower, ymax=upper, colour=sig)) + geom_point(size=5)

g1+
	deets2+
	ylab("Standardized effect size")+
	xlab("  Socio-economic                 Biophysical")+
	ylim(c(-1.5,1.5))+
	geom_errorbar(position = position_dodge(width = 0.2), width = 0.2) +
	coord_flip() +
	scale_x_discrete(breaks = est$Parameter, labels=est$labl )+
	geom_hline(yintercept=0, linetype=5)+
	scale_colour_manual(values=c("#bdbdbd", "black")) 


dev.off()


