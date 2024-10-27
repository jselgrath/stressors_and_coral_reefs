# Multiple stressors on coral reefs
# goal: figure 3a
# Effects are standardized
#############################


##############################
# Load models##############

remove(list=ls())

loc=("C:/wd/Resilience/")
setwd(loc)

dateToday<- Sys.Date()

# load final model output
load("./results/RS_only/Q41_model_52.R")

############################
# calculate se and CI######

# http://www.talkstats.com/showthread.php/25420-Confidence-Interval-for-model-parameters-(in-R-lme4)

models<-list(m.me_52)
models2<-c("m.me_52")
models3<-c( "(a) Multi-year fishing, interaction")

summary(m.me_52)

# run this before loop
est<-data.frame()

# calculating lower and upper confidence interval using the sd of the estimate
# mean model estimate: coef(summary(models[[1]]))[,1]
# model sd: coef(summary(models[[1]]))[,2]
lower <- coef(summary(models[[1]]))[,1] + qnorm(.025)*coef(summary(models[[1]]))[,2]
upper <- coef(summary(models[[1]]))[,1] + qnorm(.975)*coef(summary(models[[1]]))[,2]

# combine
e1<-data.frame(cbind(coef(summary(models[[1]])), lower, upper))
e1$Parameter<-row.names(e1)
e1$model<-models2[1]
e1$grph<-models3[1]
est<-rbind(est,e1)

rownames(est) <- c()
est

est$grph<-as.factor(est$grph)
levels(est$grph)

est$Parameter<-as.factor(est$Parameter)
levels(est$Parameter)

est

#
# Make patch shape postive
est$Estimate[est$Parameter=="Patch.shape"]<-est$Estimate[est$Parameter=="Patch.shape"]*-1
est$lower[est$Parameter=="Patch.shape"]<-est$lower[est$Parameter=="Patch.shape"]*-1
est$upper[est$Parameter=="Patch.shape"]<-est$upper[est$Parameter=="Patch.shape"]*-1

# order the varaibles sensically
est$ordr<--1*(c(1,7,8,6,5,4,3,2,9,10))

est$Parameter<-reorder(as.factor(est$Parameter), as.numeric(est$ordr), FUN=mean)
levels(est$Parameter)
dplyr::select(est,ordr, Parameter, model)

# create labels for graphs
est$labl<-as.character(est$Parameter)
est$labl<-gsub("Population.density","Population density",est$labl)
est$labl<-gsub("MPA601","MPA",est$labl)
est$labl<-gsub("Market.proximity","Market proximity",est$labl)
est$labl<-gsub("Patch.shape","Patch compactness",est$labl) # changing from shape to compactness and making positive below
est$labl<-gsub("Seagrass.isolation","Seagrass isolation",est$labl)
est$labl<-gsub("Fishing.legacy.1980.2000","Fishing legacy, 1980-2000",est$labl)
est$labl<-gsub("Fishing.legacy.1980.2000:Population.density","Fishing legacy, 1980-2000:Population density",est$labl) 
est$labl<-gsub("z.Lcum_blast10","Blast fishing, 2000-2010",est$labl)
est$labl<-gsub("Population density:Blast fishing, 2000-2010","Blast fishing, 2000-2010:Population density", est$labl)
# check labels
est$labl

# make column for insignificant values
est$sig<-1
est$sig[est$labl == "Population density" & est$model=="m.me_52"]<-0
est$sig[est$labl == "Fishing legacy, 1980-2000" & est$model=="m.me_52"]<-0
est$sig<-as.factor(est$sig)

est


# remove intercept for graph
est2<-filter(est,Parameter!="(Intercept)")



#############################
# graphing setup##############

deets2<- theme(
	axis.text =  element_text(size=rel(2.8), colour="black"), 
	axis.title=element_text(size=rel(3),colour="black"), 
	panel.grid=element_blank(), 
	# panel.grid.major=element_line(colour = "white"),#element_blank(),
	panel.background=element_rect(fill="#f7f7f7"),
	panel.border=element_rect(fill=NA, colour = "black"),
	# axis.line =  element_line(size=2),
	# axis.ticks = element_line(size=1),
	legend.position="none"
)


########################
# Graphing##############
ppi=300
tiff(paste("./doc/Fig3a_BlPop_",dateToday,"_%d.tiff",sep=""), width = (4)*ppi, height=(2)*ppi)

# est2 doesn't have intercept
g1<- 	ggplot(data = est2, aes(x=Parameter, y = Estimate,ymin=lower, ymax=upper, colour=sig)) + geom_point(size=5)

g1+
	deets2+
	ylab("Standardized effect size")+
	xlab("Socio-economic        Biophysical")+
	ylim(c(-1.5,1.5))+
	geom_errorbar(position = position_dodge(width = 0.2), width = 0.2) +
	coord_flip() + #flips graph
	scale_x_discrete(breaks = est$Parameter, labels=est$labl )+
	geom_hline(yintercept=0, linetype=5)+
	scale_colour_manual(values=c("#bdbdbd", "black"))
# annotate("text", x=.6,y=.035, label="Biophysical", size=10) 


dev.off()

################
