setwd("~/Box Sync/Graduate School/Lassen Snowmelt & Phenology/Data")

###### Merging data files #########

phendata2017=read.csv("lassenphendata2017_ES20171112.csv")
phendata2017_6_7=read.csv("phen2017_6_7.csv")

#entered data for trips 6 & 7 separately, so need to merge by uniqueID
fullphen2017 = merge(phendata2017, phendata2017_6_7, by.x="uniqueID", by.y="uniqueID_6.7", all.x=TRUE, all.y=TRUE)
write.csv(fullphen2017, '2017data.csv')

phen=read.csv("2017data.csv")
library(ggplot2)
library(plyr)
library(reshape2)

###### Data manipulation ########

phen <- subset(phen, phen.1!= "D") #rewrite phen dataframe as a subset of itself that doesn't include rows for which phen.1 = D (removing plants that were dead from previous year so the bud/flr/fruit counts aren't skewed towards lower numbers, since these plants will have counts of zero throughout the season)

phenm=melt(phen,id.vars=c("uniqueID","site","emerg.date","quad","ind"),measure.vars = c(grep("^flrs",names(phen),value = TRUE),grep("^buds",names(phen),value = TRUE),grep("^fruits",names(phen),value = TRUE))) #changed data from wide to long format. measure.vars are counted in the value column.

#colnames(phenm)[7] <- c('date')
names(phenm)
phenm$date
head(phenm)
dim(phenm)

phenm$site=factor(phenm$site,levels=c("LV3","LV2","LV1","LVTR1","LVTR2","LVTR2.5")) #tells R how to order the factors for site (did in order of increasing elevation)

phenm$value2=as.numeric(phenm$value)

phenm$date <- sapply(strsplit(as.character(phenm$variable), split='_', fixed=TRUE), function(x) (x[2])) #somehow I think this turns i.e. "date.1" into "date_06-30-17", but not sure how
phenm$date <- as.Date(phenm$date,"%d.%m.%Y") #tells R to recognize this column as dates, and what format the dates are in to begin with

######### SOMETHING IS WRONG WITH THE REPRO VALUES #############
phenm$repro <- sapply(strsplit(as.character(phenm$variable), split='_', fixed=TRUE), function(x) (x[1])) #making a vector of the "_flrs" (or buds, fruits) portion of the "date_flrs" column and splitting by the "_"
phenm$repro=as.factor(phenm$repro) #now, putting the vector into its own column, called "repro"

###### Plotting averages across sites using ggplot #######

ggplot(phenm,aes(x=date,y=value,group=repro,color=repro)) + #grouping by repro allows us to make three lines, each with a different repro category (flr, fruit, bud), and coloring those lines differently. ggplot makes a key automatically using this info
  ylim(0,12) + #setting a manual scale limit for the y axis
  scale_color_manual(values=c('seagreen','orange','orchid4')) +
  stat_summary(fun.y=mean, geom="line") + #plotting the means with points
  stat_summary(fun.data = mean_se, geom = "errorbar",width=.5) +
  stat_summary(fun.y=mean, geom="point") + #plotting the means as a line graph
  facet_wrap(~site)

####### Plotting individuals across sites #######

ggplot(phenm,aes(x=date,y=value2, color=variable, group=ind)) +
  geom_point() + #plotting the inds as points
  geom_line() + #plotting the inds as a line graph
  facet_wrap(~site)
#data=phenm, x=date, y=value2

####Subset:
phenm2 <- subset(phenm, repro="flrs", drop=TRUE)
####Jenny subset method:
phenm2 <- droplevels(phenm[phenm$repro == "flrs",])
summary(phenm2)
dim(phenm)
dim(phenm2)
levels(phenm$repro)

ggplot(phenm2,aes(x=date,y=value2, color=ind, group=ind)) +
  geom_point() + #plotting the inds as points
  geom_line() + #plotting the inds as a line graph
  facet_wrap(~site)

####### Other code from Alec ######

h1=ggplot(data=phen) +
  geom_bar(mapping=aes(x=phen.1))

h2=ggplot(data=phen) +
  geom_bar(mapping=aes(x=phen.2))

h3=ggplot(data=phen) +
  geom_bar(mapping=aes(x=phen.3))

h4=ggplot(data=phen) +
  geom_bar(mapping=aes(x=phen.4))

h5=ggplot(data=phen) +
  geom_bar(mapping=aes(x=phen.5))

h8=ggplot(data=phen) +
  geom_bar(mapping=aes(x=phen.8))

head(phen)
names(phen)

#par(mfrow=c(3,3))
library(grid)
library(gridExtra)
grid.arrange(h1,h2,h3,h4,h5,h8)

####### Alec's germination stratification graphs ########
dat <- read.csv('D_allgerm_AC20170104.csv')
datm <- melt(dat,id.vars=c("lab","tray",'treatment','pop','mf','seeds'),measure.vars = grep("^germ", names(dat), value = TRUE))
names(datm)
datm

colnames(datm)[7:8] <- c('date','germ')
datm$germ <- as.numeric(datm$germ)
datm$date <- sapply(strsplit(as.character(datm$date), split='_', fixed=TRUE), function(x) (x[2]))
datm$date <- as.Date(datm$date)
datm$repro <- sapply(strsplit(as.character(datm$date), split='_', fixed=TRUE), function(x) (x[1]))

#plot using means
ggplot(datme,aes(x=date.factor,y=germ.p,group=treatment,color=treatment)) +
  stat_summary(fun.y=mean, geom="line") +
  #geom_vline(xintercept=which(levels(datme$date.factor)=='2016-12-06'),color='gold',lwd=1.3) +
  #stat_summary(fun.y=mean, geom="line") +
  stat_summary(fun.data = mean_se, geom = "errorbar",width=.5) +
  stat_summary(fun.y=mean, geom="point") +
  scale_color_manual(values=c('red','orange','skyblue','darkblue')) +
  scale_y_continuous(breaks=1:10,labels=1:10) +
  scale_x_discrete(labels=substring(levels(datme$date.factor),6,10)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),text=element_text(size=10)) +
  facet_wrap(~pop,scales="free_x")

